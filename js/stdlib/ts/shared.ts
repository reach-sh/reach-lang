import crypto from 'crypto';
import ethers from 'ethers';
import { CBR_Address } from './CBR';
import util from 'util';

// ****************************************************************************
// Type Definitions
// ****************************************************************************

export interface AnyBackendTy {
  name: string,
  canonicalize: (x: any) => any,
}

type BigNumber = ethers.BigNumber;

type num = BigNumber | number

export type IBackendViewInfo<ConnectorTy extends AnyBackendTy> = {
  ty: ConnectorTy,
  decode: (i:number, svs:Array<any>, args:Array<any>) => any,
};

export type IBackendViewsInfo<ConnectorTy extends AnyBackendTy> =
  {[viewi: number]: Array<ConnectorTy>};
export type IBackendViews<ConnectorTy extends AnyBackendTy> = {
  views: IBackendViewsInfo<ConnectorTy>,
  infos: {[viewn: string]:
    {[keyn: string]: IBackendViewInfo<ConnectorTy>}},
};

export type IBackend<ConnectorTy extends AnyBackendTy> = {
  _getViews: (stdlib:Object) => IBackendViews<ConnectorTy>,
};

export const getViewsHelper =
  <ConnectorTy extends AnyBackendTy, B>(views:IBackendViews<ConnectorTy>, getView1:((views:IBackendViewsInfo<ConnectorTy>, v:string, k:string, vi:IBackendViewInfo<ConnectorTy>) => B)) =>
    () =>
      objectMap(views.infos, ((v:string, vm:{[keyn:string]: IBackendViewInfo<ConnectorTy>}) =>
        objectMap(vm, ((k:string, vi:IBackendViewInfo<ConnectorTy>) =>
            getView1(views.views, v, k, vi)))));

export type OnProgress = (obj: {currentTime: BigNumber, targetTime: BigNumber}) => any;

export type WPArgs = {
  host: string | undefined,
  port: number,
  output: 'silent',
  timeout: number,
}

export type MkPayAmt<Token> =
  [ BigNumber, Array<[BigNumber, Token]> ];

export type IRecvNoTimeout<RawAddress> =  {
  didTimeout: false,
  data: Array<any>,
  from: RawAddress,
  time: BigNumber,
  getOutput: (o_lab:string, o_ctc:any) => Promise<any>,
};

export type IRecv<RawAddress> = IRecvNoTimeout<RawAddress> | {
  didTimeout: true
}

export type IContract<ContractInfo, Digest, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  creationTime: () => Promise<BigNumber>,
  sendrecv: (
    funcNum: number, evt_cnt: number, hasLastTime: (BigNumber | false),
    tys: Array<ConnectorTy>,
    args: Array<any>, value: MkPayAmt<Token>, out_tys: Array<ConnectorTy>,
    onlyIf: boolean, soloSend: boolean,
    timeout_delay: BigNumber | false, sim_p: (fake: IRecv<RawAddress>) => Promise<ISimRes<Digest, RawAddress, Token, ConnectorTy>>,
  ) => Promise<IRecv<RawAddress>>,
  recv: (
    okNum: number, ok_cnt: number, out_tys: Array<ConnectorTy>,
    waitIfNotPresent: boolean,
    timeout_delay: BigNumber | false,
  ) => Promise<IRecv<RawAddress>>,
  wait: (delta: BigNumber) => Promise<BigNumber>,
  iam: (some_addr: RawAddress) => RawAddress,
  selfAddress: () => CBR_Address, // Not RawAddress!
  getViews: () => {[key: string]: {[key: string]: (() => Promise<any>)}},
  stdlib: Object,
};

type ContractIndex = 'getInfo' | 'creationTime' | 'sendrecv' | 'recv' | 'wait' | 'iam' | 'selfAddress' | 'getViews' | 'stdlib';

export const deferContract =
  <ContractInfo, Digest, RawAddress, Token, ConnectorTy extends AnyBackendTy>(
    shouldError: boolean,
    implP:Promise<IContract<ContractInfo, Digest, RawAddress, Token, ConnectorTy>>,
    implNow: Partial<IContract<ContractInfo, Digest, RawAddress, Token, ConnectorTy>>):
  IContract<ContractInfo, Digest, RawAddress, Token, ConnectorTy> => {

  const not_yet = (which:ContractIndex) => (...args: any[]): any => {
    void(args);
    throw Error(`Cannot ${which} yet; contract is not actually deployed`);
  };
  const delay = (which:ContractIndex) => async (...args: any[]): Promise<any> =>
    // @ts-ignore
    (await implP)[which](...args);
  const thenow = shouldError ? not_yet : delay;
  const mnow = (which:ContractIndex) =>
    implNow[which] === undefined ? thenow(which) : implNow[which];

  // impl starts with a shim that deploys on first sendrecv,
  // then replaces itself with the real impl once deployed.
  let impl: IContract<ContractInfo, Digest, RawAddress, Token, ConnectorTy> = {
    getInfo: delay('getInfo'),
    // @ts-ignore
    creationTime: delay('creationTime'),
    // @ts-ignore
    sendrecv: mnow('sendrecv'),
    // @ts-ignore
    recv: mnow('recv'),
    // @ts-ignore
    wait: not_yet('wait'),
    // @ts-ignore
    iam: mnow('iam'),
    // @ts-ignore
    selfAddress: mnow('selfAddress'),
    // @ts-ignore
    getViews: not_yet('getViews'),
    stdlib: (() => {
      if ( implNow.stdlib === undefined ) {
        throw Error(`stdlib not defined`); }
      return implNow.stdlib; })(),
  };

  implP.then((x) => { impl = x; });

  const wrap = (which:ContractIndex) => (...args: any[]): any =>
    // @ts-ignore
    impl[which](...args);

  // Return a wrapper around the impl. This obj and its fields do not mutate,
  // but the fields are closures around a mutating ref to impl.
  return {
    sendrecv: wrap('sendrecv'),
    recv: wrap('recv'),
    wait: wrap('wait'),
    getInfo: wrap('getInfo'),
    creationTime: wrap('creationTime'),
    iam: wrap('iam'),
    selfAddress: wrap('selfAddress'),
    getViews: wrap('getViews'),
    stdlib: impl.stdlib,
  };
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo> = {
  networkAccount: NetworkAccount,
  deploy: (bin: Backend) => Contract,
  attach: (bin: Backend, ctcInfoP: Promise<ContractInfo>) => Contract,
  stdlib: Object,
  getAddress: () => string,
  setDebugLabel: (lab: string) => IAccount<NetworkAccount, Backend, Contract, ContractInfo>,
}

export type IAccountTransferable<NetworkAccount> = IAccount<NetworkAccount, any, any, any> | {
  networkAccount: NetworkAccount,
}

export type ISimRes<Digest, RawAddress, Token, ConnectorTy> = {
  prevSt: Digest,
  prevSt_noPrevTime: Digest,
  txns: Array<ISimTxn<RawAddress, Token>>,
  nextSt: Digest,
  nextSt_noTime: Digest,
  view: [ ConnectorTy, any ],
  isHalt : boolean,
};

export type ISimTxn<RawAddress, Token> = {
  kind: "to"|"init",
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: "from",
  to: RawAddress,
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: "halt",
  tok: Token|undefined,
};

export type CurrencyAmount = string | number | BigNumber

export type {Connector} from './ConnectorMode';

// ****************************************************************************
// Helpers
// ****************************************************************************

let DEBUG: boolean = process.env.REACH_DEBUG ? true : false;

const {
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;

const BigNumber = ethers.BigNumber;

// Hex helpers
// const un0x           = h => h.replace(/^0x/, ''); // unused
const hexTo0x = (h: string): string => '0x' + h.replace(/^0x/, '');

const byteToHex = (b: number): string => (b & 0xFF).toString(16).padStart(2, '0');

const byteArrayToHex = (b: any): string => Array.from(b, byteToHex).join('');

const format_ai = (ai: any) => JSON.stringify(ai);

const forceHex = (x: string): string =>
  isHex(x) ? x : stringToHex(x);

// ****************************************************************************
// Utility exports
// ****************************************************************************

export function truthyEnv(v: string|undefined|null): v is string {
  if (!v) return false;
  return ![
    '0', 'false', 'f', '#f', 'no', 'off',
  ].includes(v && v.toLowerCase && v.toLowerCase());
}

export const envDefault = <T>(v: string|undefined|null, d: T): string|T =>
  (v === undefined || v === null) ? d : v;

export const setDEBUG = (b: boolean) => {
  if (b === false || b === true) {
    DEBUG = b;
  } else {
    throw Error(`Expected bool, got ${JSON.stringify(b)}`);
  }
};

export const getDEBUG = (): boolean => { return DEBUG; };

export const debug = (...msgs: any) => {
  if (getDEBUG()) {
    // Print arrays/objects in full instead of the default depth of 2
    const betterMsgs = msgs.map((msg: any) =>
      ["object", "array"].includes(typeof msg)
        ? util.inspect(msg, false, null, true)
        : msg);
    void(betterMsgs);
    // Print objects for indentation, colors, etc...
    console.log(new Date(), `DEBUG:`, ...msgs);
  }
};

export const assert = (d: any, ai: any = null) => {
  if (!d) {
    throw Error(format_ai(ai));
  }
}

export const { isBigNumber } = BigNumber;

export const bigNumberify = (x: any): BigNumber => BigNumber.from(x);

export const bigNumberToNumber = (x: any) =>
  bigNumberify(x).toNumber();

export const checkedBigNumberify = ( at:string, m:BigNumber, x:any ): BigNumber => {
  const xb = bigNumberify(x);
  if (xb.gte(0) && xb.lte(m)) {
    return xb;
  }
  throw Error(`bigNumberify: ${x} out of range [0, ${m}] at ${at}`);
};

// Contracts

// .canonicalize turns stuff into the "canonical backend representation"
export function protect (ctc: AnyBackendTy, v: unknown, ai: unknown = null) {
  try {
    return ctc.canonicalize(v);
  } catch (e) {
    console.log(`Protect failed: expected ${ctc.name} but got ${JSON.stringify(v)} ${format_ai(ai)}`);
    throw e;
  }
}

export const isHex = isHexString;

export const hexToString = toUtf8String;

export const stringToHex = (x:string): string =>
  hexlify(toUtf8Bytes(x));

export const makeDigest = (prep: any) => (t:any, v:any) => {
  const args = [t, v];
  debug('digest(', args, ') =>');
  const kekCat = prep(t, v);
  debug('digest(', args, ') => internal(', hexlify(kekCat), ')');
  const r = ethers.utils.keccak256(kekCat);
  debug('keccak(', args, ') => internal(', hexlify(kekCat), ') => ', r);
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

export const bytesEq = (x: any, y: any): boolean => {
  debug('bytesEq(', x, ',', y, ')');
  return forceHex(x) === forceHex(y); };

export const digestEq = bytesEq;

export const makeRandom = (width:number) => {
  const randomUInt = (): BigNumber =>
    hexToBigNumber(byteArrayToHex(crypto.randomBytes(width)));

  const hasRandom = {
    random: randomUInt,
  };

  return { randomUInt, hasRandom };
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

export const argsSlice = <T>(args: Array<T>, cnt: number): Array<T> =>
  cnt == 0 ? [] : args.slice(-1 * cnt);

export const argsSplit = <T>(args: Array<T>, cnt: number): [ Array<T>, Array<T> ] =>
  cnt == 0 ? [args, []] : [ args.slice(0, args.length - cnt), args.slice(-1 * cnt) ];

export function Array_set <T>(arr: Array<T>, idx: number, elem: T): Array<T> {
  const arrp = arr.slice();
  arrp[idx] = elem;
  return arrp;
}

export const Array_zip = <X,Y>(x: Array<X>, y: Array<Y>): Array<[X, Y]> =>
  x.map((e, i): [X, Y] => [e, y[i]]);

export const mapRef = (m: any, f: any): any => {
  const v = m[f];
  // console.log(`Reading map ${JSON.stringify(m)} field ${JSON.stringify(f)} => ${JSON.stringify(v)}`);
  if ( v === undefined ) {
    return ['None', null];
  } else {
    return ['Some', v];
  }
};

export const objectMap = <A,B>(object: {[key:string]: A}, mapFn: ((k:string, a:A) => B)): {[key:string]: B} =>
  Object.keys(object).reduce(function(result: {[key:string]: B}, key:string) {
    result[key] = mapFn(key, object[key])
    return result;
  }, {});


// XXX this doesn't really belong here, but hard to relocate due to dep on bytesEq
export const mkAddressEq = (T_Address: {canonicalize: (addr:any) => any}
) => (x:any, y:any): boolean =>
  bytesEq(T_Address.canonicalize(x), T_Address.canonicalize(y));

export const parseFixedPoint = (x: { sign: boolean, i: { i: num, scale: num } }): number =>
  parseInt({ sign: x.sign, i: x.i.i }) / bigNumberify(x.i.scale).toNumber();

export const parseInt = (x: { sign: boolean, i: num}) =>
  bigNumberify(x.i).toNumber() * (x.sign ? 1 : (- 1));
