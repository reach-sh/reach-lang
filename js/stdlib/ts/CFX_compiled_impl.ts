import * as eci from './ETH_compiled_impl';
import cfxsdk from 'js-conflux-sdk';
import type { ETH_Ty } from './ETH_like_interfaces';

// XXX find a better way to support multiple netIds
let netId = 999;

function address_ethToCfx(addrE: string): string {
  addrE = addrE.toLowerCase();
  const addrB = Buffer.from(addrE.slice(2), 'hex');
  // XXX why doesn't ts know about this fn?
  const addrC = (cfxsdk.address as any).encodeCfxAddress(addrB, netId);

  const pieces = addrC.split(':');
  // XXX Missing type chunk means assume it's a user (?)
  // XXX would it be better for our purposes to strip the type out instead?
  if (pieces.length === 2) {
    return `${pieces[0]}:TYPE.USER:${pieces[1]}`.toUpperCase();
  }
  // XXX throw error if pieces.length isn't 2 or 3?
  if (pieces.length !== 3) throw Error(`impossible: bad CFX addr: '${addrC}'`);

  return addrC.toUpperCase();
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
        return addrC.toUpperCase();
      }
      return uv.toUpperCase();
    }
    return uv as string; // XXX
  },
  defaultValue: 'XXX', // XXX
  // Note: address_cfxToEth is not strictly necessary for munge.
  // ((x) => x) also seems to work.
  // But perhaps CBR for CFX should be the hex string, more like ETH?
  // Will the CFX apis someday reject addresses in hex format? Probably not?
  munge: (bv: string): string => address_cfxToEth(bv),
  unmunge: (nv: string): string => T_Address.canonicalize(nv),
}
