import * as eci from './ETH_compiled_impl';
import type { ETH_Ty } from './ETH_like_interfaces';
import buffer from 'buffer';
import { address_cfxStandardize, decodeCfxAddress, encodeCfxAddress } from './CFX_util';
import { debug } from './shared_impl';

const { Buffer } = buffer;

// XXX find a better way to support multiple netIds
let netId = 999;

function address_ethToCfx(addrE: string): string {
  debug(`address_ethToCfx`, `call`, addrE);
  addrE = addrE.toLowerCase();
  const addrB = Buffer.from(addrE.slice(2), 'hex');
  const addrC = encodeCfxAddress(addrB, netId);
  return addrC;
}

// Note: does not add the mixed-case checksum info to the ETH-like address
function address_cfxToEth(addrC: string): string {
  // XXX why doesn't ts know about this fn?
  debug(`address_cfxToEth`, `call`, addrC);
  const addrObj = decodeCfxAddress(addrC);
  const addrE = '0x' + addrObj.hexAddress.toString('hex');
  if (netId !== addrObj.netId) throw Error(`Expected netId=${netId}, got netId=${addrObj.netId}`);
  return addrE;
}

export const T_Address: ETH_Ty<string, string> = {
  ...eci.T_Address,
  canonicalize: (uv: unknown): string => {
    debug(`address canonicalize`, {uv});
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
  // XXX this should not be computed at compile time, because it can change based on the netId
  // Note: 0x1 = user, 0x8 = contract
  // https://github.com/resodo/conflux-address-js/blob/0cbbe3d17fbd6cbc2c2fbafc3470ff6087f38087/lib/index.js#L86
  // defaultValue: address_cfxStandardize(address_ethToCfx(eci.T_Address.defaultValue.replace('0x0', '0x1'))),
  // XXX I (Dan) would like to address_cfxStandardize here, but I can't figure out how to disentangle defaultValue
  // so that this addr defaultValue can be different than its ETH-equivalent defaultValue.
  // (0x0 is not considered a valid addr prefix for the new cfx addr style)
  defaultValue: address_ethToCfx(eci.T_Address.defaultValue),
  // Note: address_cfxToEth is not strictly necessary for munge.
  // ((x) => x) also seems to work.
  // But perhaps CBR for CFX should be the hex string, more like ETH?
  // Will the CFX apis someday reject addresses in hex format? Probably not?
  munge: (bv: string): string => address_cfxToEth(bv),
  unmunge: (nv: string): string => T_Address.canonicalize(nv),
}
