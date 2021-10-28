import { encode, decode } from './cfxaddr_index';
import type { EpochNumber } from 'js-conflux-sdk';
import { debug } from './shared_impl';

/** @description Precondition: addrC is a valid Conflux address */
export function address_cfxStandardize(addrC: string): string {
  debug(`address_cfxStandardize`, {addrC});
  const pieces = addrC.split(':');
  //debug(`address_cfxStandardize`, pieces.length, {pieces});
  if (pieces.length === 3) {
    addrC = `${pieces[0]}:${pieces[2]}`;
  } else if (pieces.length !== 2) {
    throw Error(`impossible: bad CFX addr: '${addrC}'`);
  }
  //debug(`address_cfxStandardize`, {addrC});
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
