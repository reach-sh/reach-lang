import crypto from 'crypto';
import ethers from 'ethers';
import { labelMaps } from './shared_impl';

export interface AnyBackendTy {
  name: string,
  canonicalize: (x: any) => any,
}
type BigNumber = ethers.BigNumber;
type num = BigNumber | number

const BigNumber = ethers.BigNumber;

export type OnProgress = (obj: {currentTime: BigNumber, targetTime: BigNumber}) => any;
export type WPArgs = {
  host: string | undefined,
  port: number,
  output: 'silent',
  timeout: number,
}

export type IRecvNoTimeout<RawAddress> =  {
  didTimeout: false,
  data: Array<any>,
  value: BigNumber,
  from: RawAddress,
};

export type IRecv<RawAddress> = IRecvNoTimeout<RawAddress> | {
  didTimeout: true
}

export type IContract<ContractInfo, Digest, RawAddress, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  sendrecv: (
    label: string, funcNum: number, evt_cnt: number, tys: Array<ConnectorTy>,
    args: Array<any>, value: BigNumber, out_tys: Array<ConnectorTy>,
    timeout_delay: BigNumber | false, sim_p: (fake: IRecv<RawAddress>) => ISimRes<Digest, RawAddress>,
  ) => Promise<IRecv<RawAddress>>,
  recv: (
    label: string, okNum: number, ok_cnt: number, out_tys: Array<ConnectorTy>,
    timeout_delay: BigNumber | false,
  ) => Promise<IRecv<RawAddress>>,
  wait: (delta: BigNumber) => Promise<BigNumber>,
  iam: (some_addr: RawAddress) => RawAddress,
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo> = {
  networkAccount: NetworkAccount,
  deploy: (bin: Backend) => Contract,
  attach: (bin: Backend, ctc: ContractInfo | Promise<ContractInfo>) => Contract,
}

export type IAccountTransferable<NetworkAccount> = IAccount<NetworkAccount, any, any, any> | {
  networkAccount: NetworkAccount,
}

export type ISimRes<Digest, RawAddress> = {
  prevSt: Digest,
  txns: Array<ISimTxn<RawAddress>>,
  nextSt: Digest,
  isHalt : boolean,
};
export type ISimTxn<RawAddress> = {
  to: RawAddress,
  amt: BigNumber,
};

let DEBUG: boolean = process.env.REACH_DEBUG ? true : false;
export const setDEBUG = (b: boolean) => {
  if (b === false || b === true) {
    DEBUG = b;
  } else {
    throw Error(`Expected bool, got ${JSON.stringify(b)}`);
  }
};
export const getDEBUG = (): boolean => { return DEBUG; };
export const debug = (msg: any) => {
  if (getDEBUG()) {
    console.log(`[${(new Date()).toISOString()}] DEBUG: ${msg}`);
  }
};

export const assert = (d: any, ai: any = null) => {
  if (!d) {
    throw Error(format_ai(ai));
  }
}

const {
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;
export const { isBigNumber } = BigNumber;
export const bigNumberify = (x: any): BigNumber => BigNumber.from(x);

export const checkedBigNumberify = ( at:string, m:BigNumber, x:any ): BigNumber => {
  const xb = bigNumberify(x);
  if (xb.gte(0) && xb.lte(m)) {
    return xb;
  }
  throw Error(`bigNumberify: ${x} out of range [0, ${m}] at ${at}`);
};

// Hex helpers
// const un0x           = h => h.replace(/^0x/, ''); // unused
const hexTo0x = (h: string): string => '0x' + h.replace(/^0x/, '');
const byteToHex = (b: number): string => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = (b: any): string => Array.from(b, byteToHex).join('');

// const hexOf = x =>
//       typeof x === 'string' && x.slice(0, 2) === '0x'
//       ? un0x(toHex(x))
//       : un0x(toHex(`0x${x}`));
const hexOf = (x: any): string => toHex(x);
// TODO: why was this stripping off the 0x?
// Why was it slapping 0x on non-hex strings?


// Contracts

// .canonicalize turns stuff into the "canonical backend representation"

const format_ai = (ai: any) => JSON.stringify(ai);

export function protect (ctc: AnyBackendTy, v: unknown, ai: unknown = null) {
  try {
    return ctc.canonicalize(v);
  } catch (e) {
    console.log(`Protect failed: expected ${ctc.name} but got ${JSON.stringify(v)} ${format_ai(ai)}`);
    throw e;
  }
}

// Massage the arg into a form keccak256 will handle correctly
let digestWidth = 32;
export const setDigestWidth = (sz: number): void => {
  digestWidth = sz;
};
const kek = (arg: any): string | Uint8Array => {
  if (typeof(arg) === 'string') {
    if (isHex(arg)) {
      return arg;
    } else {
      return toUtf8Bytes(arg);
    }
  } else if (typeof(arg) === 'boolean') {
    return kek(arg ? 1 : 0);
  } else if (typeof(arg) === 'number') {
    return '0x' + bigNumberToHex(arg, digestWidth);
  } else if (isBigNumber(arg)) {
    return '0x' + bigNumberToHex(arg, digestWidth);
  } else if (arg && arg.constructor && arg.constructor.name == 'Uint8Array') {
    return arg;
  } else if (arg && arg.constructor && arg.constructor.name == 'Buffer') {
    return '0x' + arg.toString('hex');
  } else if (Array.isArray(arg)) {
    return ethers.utils.concat(arg.map((x) => ethers.utils.arrayify(kek(x))));
  } else if (Object.keys(arg).length > 0) {
    const {ascLabels} = labelMaps(arg);
    return kek(ascLabels.map((label => arg[label])));
  } else {
    throw Error(`Can't kek this: ${JSON.stringify(arg)}`);
  }
};

export const toHex = (x: any) => hexlify(kek(x));
export const isHex = isHexString;
export const hexToString = toUtf8String;

// XXX the JS backend expects this to be a BigNumber
export const digest = (...args: Array<any>) => {
  debug(`digest(${JSON.stringify(args)}) =>`);
  const kekCat = kek(args);
  debug(`digest(${JSON.stringify(args)}) => internal(${hexlify(kekCat)})`);
  const r = ethers.utils.keccak256(kekCat);
  debug(`keccak(${JSON.stringify(args)}) => internal(${hexlify(kekCat)} => ${JSON.stringify(r)}`);
  return r;
};

export const hexToBigNumber = (h: string): BigNumber => bigNumberify(hexTo0x(h));
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

export const bytesEq = (x: any, y: any): boolean =>
  hexOf(x) === hexOf(y);
export const digestEq = bytesEq;

export const randomUInt = (): BigNumber =>
  hexToBigNumber(byteArrayToHex(crypto.randomBytes(digestWidth)));

export const hasRandom = {
  random: randomUInt,
};

export const eq = (a: num, b: num): boolean => bigNumberify(a).eq(bigNumberify(b));
export const add = (a: num, b: num): BigNumber => bigNumberify(a).add(bigNumberify(b));
export const sub = (a: num, b: num): BigNumber => bigNumberify(a).sub(bigNumberify(b));
export const mod = (a: num, b: num): BigNumber => bigNumberify(a).mod(bigNumberify(b));
export const mul = (a: num, b: num): BigNumber => bigNumberify(a).mul(bigNumberify(b));
export const div = (a: num, b: num): BigNumber => bigNumberify(a).div(bigNumberify(b));
export const ge = (a: num, b: num): boolean => bigNumberify(a).gte(bigNumberify(b));
export const gt = (a: num, b: num): boolean => bigNumberify(a).gt(bigNumberify(b));
export const le = (a: num, b: num): boolean => bigNumberify(a).lte(bigNumberify(b));
export const lt = (a: num, b: num): boolean => bigNumberify(a).lt(bigNumberify(b));

// Array helpers

export function Array_set <T>(arr: Array<T>, idx: number, elem: T): Array<T> {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp;
}

export const Array_zip = <X,Y>(x: Array<X>, y: Array<Y>): Array<[X, Y]> =>
  x.map((e, i): [X, Y] => [e, y[i]]);

export type CurrencyAmount = string | number | BigNumber
export type {Connector} from './ConnectorMode';

// XXX this doesn't really belong here, but hard to relocate due to dep on bytesEq
export const mkAddressEq = (T_Address: {canonicalize: (addr:any) => any}
) => (x:any, y:any): boolean =>
  bytesEq(T_Address.canonicalize(x), T_Address.canonicalize(y));
