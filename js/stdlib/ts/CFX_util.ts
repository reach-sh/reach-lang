import { encode, decode } from './cfxaddr_index';
import type { EpochNumber } from 'js-conflux-sdk';

// XXX check if networkId is "correct"?
export function address_cfxStandardize(addrC: string): string {
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

// mimicking cfxsdk.address.encodeCfxAddress
export function encodeCfxAddress(hexAddress: Buffer, netId: number): string {
  return encode(hexAddress, netId);
}

// mimicking cfxsdk.address.decodeCfxAddress
export function decodeCfxAddress(addr: string): {hexAddress: Buffer, netId: number, type: string} {
  return decode(addr);
}

// TODO: 'latest_state' seems to work well; is there a better choice?
export const defaultEpochTag: EpochNumber = 'latest_state';
