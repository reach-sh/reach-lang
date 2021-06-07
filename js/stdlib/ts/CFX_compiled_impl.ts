import * as eci from './ETH_compiled_impl';
import cfxsdk from 'js-conflux-sdk';
import type { ETH_Ty } from './ETH_like_interfaces';
import buffer from 'buffer';
import { address_cfxStandardize } from './CFX_util';

const { Buffer } = buffer;

// XXX find a better way to support multiple netIds
let netId = 999;

function address_ethToCfx(addrE: string): string {
  addrE = addrE.toLowerCase();
  const addrB = Buffer.from(addrE.slice(2), 'hex');
  // XXX why doesn't ts know about this fn?
  const addrC = (cfxsdk.address as any).encodeCfxAddress(addrB, netId);
  return addrC;
}

// Note: does not add the mixed-case checksum info to the ETH-like address
function address_cfxToEth(addrC: string): string {
  // XXX why doesn't ts know about this fn?
  const addrObj = (cfxsdk.address as any).decodeCfxAddress(addrC);
  const addrE = '0x' + addrObj.hexAddress.toString('hex');
  if (netId !== addrObj.netId) throw Error(`Expected netId=${netId}, got netId=${addrObj.netId}`);
  return addrE;
}

export const T_Address: ETH_Ty<string, string> = {
  ...eci.T_Address,
  canonicalize: (uv: unknown): string => {
    if (typeof uv === 'string') {
      if (uv.slice(0,2) === '0x') {
        const addrC = address_ethToCfx(uv);
        return address_cfxStandardize(addrC);
      }
      return address_cfxStandardize(uv);
    }
    if (!uv) throw Error(`Expected address, got ${JSON.stringify(uv)}`);
    // XXX what's a better way to show ts what's going on?
    const uobj = uv as {networkAccount?: unknown, address?: unknown};
    if (uobj.networkAccount) {
      return T_Address.canonicalize(uobj.networkAccount);
    }
    if (uobj.address) {
      return T_Address.canonicalize(uobj.address);
    }
    throw Error(`TODO: canonicalize non-string addr`);
  },
  defaultValue: 'XXX', // XXX
  // Note: address_cfxToEth is not strictly necessary for munge.
  // ((x) => x) also seems to work.
  // But perhaps CBR for CFX should be the hex string, more like ETH?
  // Will the CFX apis someday reject addresses in hex format? Probably not?
  munge: (bv: string): string => address_cfxToEth(bv),
  unmunge: (nv: string): string => T_Address.canonicalize(nv),
}
