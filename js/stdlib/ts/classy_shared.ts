import { ReachStdlib_Opts } from './classy_opts';
import { TypeDef, TypeDefs } from './classy_TypeDefs';
import * as shared from './shared';
import { BigNumber, BytesLike } from 'ethers';
import { Utf8ErrorFunc } from 'ethers/lib/utils';
import ethers from 'ethers';
import crypto from 'crypto';
const { hexlify } = ethers.utils;

// Helpers
type num = number | BigNumber
const byteToHex = (b: number): string => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = (b: any): string => Array.from(b, byteToHex).join('');

export abstract class ReachStdlib {
  abstract readonly typeDefs: TypeDefs
  abstract readonly standardUnit: string
  // TODO: revisit the digest/prepForDigest point of abstraction
  abstract prepForDigest(t: TypeDef, v: unknown): BytesLike
  abstract tokenEq(x: unknown, y: unknown): boolean

  readonly opts: ReachStdlib_Opts;
  constructor(opts: ReachStdlib_Opts = {}) {
    opts = {
      REACH_DEBUG: false,
      REACH_CONNECTOR_MODE: 'ETH',
      ...opts,
    }
    this.opts = opts;
    // XXX We are turning a blind eye to mutation for now,
    // but later should delete setDEBUG and only set it via the opts.
    if (opts.REACH_DEBUG) this.setDEBUG(true);
  }
  /** @deprecated */
  setDEBUG(b: boolean): void { return shared.setDEBUG(b); }
  /** @deprecated */
  getDEBUG(): boolean { return shared.getDEBUG(); }
  debug(...msgs: any): void { shared.debug(...msgs); }
  assert(cond: unknown, ai: unknown = null): void { return shared.assert(cond, ai); }
  isBigNumber(x: unknown): x is BigNumber { return shared.isBigNumber(x); }
  bigNumberify(x: unknown): BigNumber { return shared.bigNumberify(x); }
  bigNumberToNumber(x: unknown): number { return shared.bigNumberToNumber(x); }
  checkedBigNumberify(at: string, m: BigNumber, x: unknown): BigNumber { return shared.checkedBigNumberify(at, m, x); }
  protect(td: TypeDef, v: unknown, ai: unknown = null): unknown { return shared.protect(td, v, ai); }
  isHex(value: unknown, length?: number): value is string { return shared.isHex(value, length); }
  hexToString(bytes: BytesLike, onError?: Utf8ErrorFunc): string { return shared.hexToString(bytes, onError); }
  stringToHex(x: string): string { return shared.stringToHex(x); }
  // makeDigest(prep: unknown): (t: unknown, v: unknown) => string { return shared.makeDigest(prep); }
  hexToBigNumber(h: string): BigNumber { return shared.hexToBigNumber(h); }
  uintToBytes(i: BigNumber): string { return shared.bigNumberToHex(i); }
  bigNumberToHex(u: num, size: number = 32) { shared.bigNumberToHex(u, size); }
  bytesEq(x: unknown, y: unknown): boolean { return shared.bytesEq(x, y); }
  digestEq(x: unknown, y: unknown): boolean { return shared.digestEq(x, y); }
  makeRandom(width: number): {
    randomUInt: () => BigNumber,
    hasRandom: { random: () => BigNumber },
  } { return shared.makeRandom(width); }

  eq(a: num, b: num): boolean { return shared.eq(a, b); }
  add(a: num, b: num): BigNumber { return shared.add(a, b); }
  sub(a: num, b: num): BigNumber { return shared.sub(a, b); }
  mod(a: num, b: num): BigNumber { return shared.mod(a, b); }
  mul(a: num, b: num): BigNumber { return shared.mul(a, b); }
  div(a: num, b: num): BigNumber { return shared.div(a, b); }
  ge(a: num, b: num): boolean { return shared.ge(a, b); }
  gt(a: num, b: num): boolean { return shared.gt(a, b); }
  le(a: num, b: num): boolean { return shared.le(a, b); }
  lt(a: num, b: num): boolean { return shared.lt(a, b); }

  argsSlice<T>(args: T[], cnt: number): T[] { return shared.argsSlice(args, cnt); }
  argsSplit<T>(args: T[], cnt: number): [T[], T[]] { return shared.argsSplit(args, cnt); }
  Array_set<T>(arr: T[], idx: number, elem: T): T[] { return shared.Array_set(arr, idx, elem); }
  Array_zip<X,Y>(x: X[], y: Y[]): [X,Y][] { return shared.Array_zip(x, y); }
  mapRef(m: unknown, f: unknown): unknown { return shared.mapRef(m, f); }

  parseFixedPoint(x: { sign: boolean, i: { i: num, scale: num } }): number { return shared.parseFixedPoint(x); }
  parseInt(x: { sign: boolean, i: num}): number { return shared.parseInt(x); }

  // Different than shared!
  addressEq(x: unknown, y: unknown): boolean {
    const {T_Address} = this.typeDefs;
    return this.bytesEq(T_Address.canonicalize(x), T_Address.canonicalize(y));
  }
  digest(t: TypeDef, v: unknown) {
    // TODO: cleaner impl
    const args = [t, v];
    this.debug('digest(', args, ') =>');
    const kekCat = this.prepForDigest(t, v);
    this.debug('digest(', args, ') => internal(', hexlify(kekCat), ')');
    const r = ethers.utils.keccak256(kekCat);
    this.debug('keccak(', args, ') => internal(', hexlify(kekCat), ') => ', r);
    return r;
  }
  randomUInt(): BigNumber {
    const {T_UInt} = this.typeDefs;
    // TODO: abstract away which `crypto.randomBytes` impl is used
    return this.hexToBigNumber(byteArrayToHex(crypto.randomBytes(T_UInt.width)));
  }
  readonly hasRandom = { random: () => this.randomUInt() }
}
