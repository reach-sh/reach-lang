// This can depend on the shared backend and impl
import { ethers } from 'ethers';
import { bigNumberify, bigNumberToNumber } from './CBR';
import {
  hexlify,
  hexToBigNumber,
} from './shared_impl';
import {
  num,
  isHex,
  stringToHex,
} from './shared_backend';
export {
  isHex,
  stringToHex,
  bigNumberify,
  bigNumberToNumber,
  hexToBigNumber
};

type BigNumber = ethers.BigNumber;

const BigNumber = ethers.BigNumber;

export const { isBigNumber } = BigNumber;

export const uintToBytes = (i: BigNumber): string => bigNumberToHex(i);

export const bigNumberToHex = (u: num, size: number = 32) => {
  const width = 8 * size;
  const format = `ufixed${width}x0`;
  const nPos = bigNumberify(u).toTwos(width);
  // They took away padZeros so we have to use FixedNumber
  const nFix = ethers.FixedNumber.from(nPos.toString(), format);
  // XXX why do we slice off the 0x?
  return hexlify(nFix).slice(2);
};

export const parseFixedPoint = (x: { sign: boolean, i: { i: num, scale: num } }): number =>
  parseInt({ sign: x.sign, i: x.i.i }) / bigNumberify(x.i.scale).toNumber();

export const parseInt = (x: { sign: boolean, i: num}) =>
  bigNumberify(x.i).toNumber() * (x.sign ? 1 : (- 1));
