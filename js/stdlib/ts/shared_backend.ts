// This has no dependencies on other shared things
import ethers from 'ethers';
import {
  bigNumberify
} from './CBR';

type BigNumber = ethers.BigNumber;
export type num = BigNumber | number

export interface AnyBackendTy {
  name: string,
  canonicalize: (x: any) => any,
};

export const assert = (d: any, ai: any = null) => {
  if (!d) {
    throw Error(JSON.stringify(ai));
  }
};

export const checkedBigNumberify = ( at:string, m:BigNumber, x:any ): BigNumber => {
  const xb = bigNumberify(x);
  if (xb.gte(0) && xb.lte(m)) {
    return xb;
  }
  throw Error(`bigNumberify: ${x} out of range [0, ${m}] at ${at}`);
};

// .canonicalize turns stuff into the "canonical backend representation"
export function protect (ctc: AnyBackendTy, v: unknown, ai: unknown = null) {
  try {
    return ctc.canonicalize(v);
  } catch (e) {
    console.log(`Protect failed: expected `, ctc.name, ` but got `, v, ` `, ai);
    throw e;
  }
};

const {
  toUtf8Bytes,
  isHexString,
} = ethers.utils;

export const hexlify = ethers.utils.hexlify;

export const isHex = isHexString;

export const stringToHex = (x:string): string =>
  hexlify(toUtf8Bytes(x));

const forceHex = (x: string): string =>
  isHex(x) ? x : stringToHex(x);

export const bytesEq = (x: any, y: any): boolean => {
  return forceHex(x) === forceHex(y); };

export const digestEq = bytesEq;

export const eq = (a: num, b: num): boolean => bigNumberify(a).eq(bigNumberify(b));
export const ge = (a: num, b: num): boolean => bigNumberify(a).gte(bigNumberify(b));
export const gt = (a: num, b: num): boolean => bigNumberify(a).gt(bigNumberify(b));
export const le = (a: num, b: num): boolean => bigNumberify(a).lte(bigNumberify(b));
export const lt = (a: num, b: num): boolean => bigNumberify(a).lt(bigNumberify(b));

export function Array_set <T>(arr: Array<T>, idx: number, elem: T): Array<T> {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp;
}
export const mapRef = (m: any, f: any): any => {
  const v = m[f];
  // console.log(`Reading map ${JSON.stringify(m)} field ${JSON.stringify(f)} => ${JSON.stringify(v)}`);
  if ( v === undefined ) {
    return ['None', null];
  } else {
    return ['Some', v];
  }
};

export const Array_zip = <X,Y>(x: Array<X>, y: Array<Y>): Array<[X, Y]> =>
  x.map((e, i): [X, Y] => [e, y[i]]);

