// This can depend on the shared backend
import crypto from 'crypto';
import { ethers } from 'ethers';
import {
  CBR_Address,
  bigNumberify,
} from './CBR';
import util from 'util';
import {
  num,
  hexlify,
  AnyBackendTy,
  checkedBigNumberify,
  bytesEq,
} from './shared_backend';
import { process } from './shim';
export {
  hexlify
} from './shared_backend';

type BigNumber = ethers.BigNumber;

export type CurrencyAmount = string | number | BigNumber

export type {Connector} from './ConnectorMode';

let DEBUG: boolean = process.env.REACH_DEBUG ? true : false;

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
      ["object", "array"].includes(typeof msg) && util && util.inspect instanceof Function
        ? util.inspect(msg, false, null, true)
        : msg);
    void(betterMsgs);
    // Print objects for indentation, colors, etc...
    console.log(new Date(), `DEBUG:`, ...msgs);
  }
};

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

export type IBackendMaps<ConnectorTy extends AnyBackendTy> = {
  mapDataTy: ConnectorTy,
};

export type IBackend<ConnectorTy extends AnyBackendTy> = {
  _getViews: (stdlib:Object) => IBackendViews<ConnectorTy>,
  _getMaps: (stdlib:Object) => IBackendMaps<ConnectorTy>,
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
  data: Array<unknown>,
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
    timeout_delay: BigNumber | false, sim_p: (fake: IRecv<RawAddress>) => Promise<ISimRes<Digest, Token, ConnectorTy>>,
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

export type ISimRes<Digest, Token, ConnectorTy> = {
  prevSt: Digest,
  prevSt_noPrevTime: Digest,
  txns: Array<ISimTxn<Token>>,
  mapRefs: Array<string>,
  mapsPrev: any,
  mapsNext: any,
  nextSt: Digest,
  nextSt_noTime: Digest,
  view: [ ConnectorTy, any ],
  isHalt : boolean,
};

export type ISimTxn<Token> = {
  kind: "to"|"init",
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: "from",
  to: string,
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: "halt",
  tok: Token|undefined,
};

/**
 * @description Create a getter/setter, where the getter defaults to memoizing a thunk
 */
export function replaceableThunk<T>(thunk: () => T): [() => T, (val: T) => void] {
  let called = false;
  let res: T | null  = null;
  function get(): T {
    if (!called) {
      called = true;
      res = thunk();
    }
    return res as T;
  }
  function set(val: T): void {
    if (called) {
      throw Error(`Cannot re-set value once already set`);
    }
    res = val;
    called = true;
  }
  return [get, set];
}

/**
 * @description Only perform side effects from thunk on the first call.
 */
export function memoizeThunk<T>(thunk: () => T): () => T {
  return replaceableThunk(thunk)[0];
}

/**
 * @description ascLabels[i] = label; labelMap[label] = i;
 */
export const labelMaps = (co: {
  [key: string]: unknown
}): {
  ascLabels: Array<string>,
  labelMap: {[key: string]: number}
} => {
  const ascLabels = Object.keys(co).sort();
  const labelMap: {
    [key: string]: number
  } = {};
  for (const i in ascLabels) {
    labelMap[ascLabels[i]] = parseInt(i);
  }
  return {ascLabels, labelMap};
}

export function truthyEnv(v: string|undefined|null): v is string {
  if (!v) return false;
  return ![
    '0', 'false', 'f', '#f', 'no', 'off',
  ].includes(v && v.toLowerCase && v.toLowerCase());
}

export const envDefault = <T>(v: string|undefined|null, d: T): string|T =>
  (v === undefined || v === null) ? d : v;

export const makeDigest = (prep: any) => (t:any, v:any) => {
  void(hexlify);
  // const args = [t, v];
  // debug('digest(', args, ') =>');
  const kekCat = prep(t, v);
  // debug('digest(', args, ') => internal(', hexlify(kekCat), ')');
  const r = ethers.utils.keccak256(kekCat);
  // debug('keccak(', args, ') => internal(', hexlify(kekCat), ') => ', r);
  return r;
};

export const hexToString = ethers.utils.toUtf8String;

const byteToHex = (b: number): string => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = (b: any): string => Array.from(b, byteToHex).join('');
const hexTo0x = (h: string): string => '0x' + h.replace(/^0x/, '');

export const hexToBigNumber = (h: string): BigNumber => bigNumberify(hexTo0x(h));

export const makeRandom = (width:number) => {
  const randomUInt = (): BigNumber =>
    hexToBigNumber(byteArrayToHex(crypto.randomBytes(width)));

  const hasRandom = {
    random: randomUInt,
  };

  return { randomUInt, hasRandom };
};


export const makeArith = (m:BigNumber) => {
  const check = (x: BigNumber) =>
    checkedBigNumberify(`internal`, m, x);
  const add = (a: num, b: num): BigNumber => check(bigNumberify(a).add(bigNumberify(b)));
  const sub = (a: num, b: num): BigNumber => check(bigNumberify(a).sub(bigNumberify(b)));
  const mod = (a: num, b: num): BigNumber => check(bigNumberify(a).mod(bigNumberify(b)));
  const mul = (a: num, b: num): BigNumber => check(bigNumberify(a).mul(bigNumberify(b)));
  const div = (a: num, b: num): BigNumber => check(bigNumberify(a).div(bigNumberify(b)));
  return { add, sub, mod, mul, div };
};

export const argsSlice = <T>(args: Array<T>, cnt: number): Array<T> =>
  cnt == 0 ? [] : args.slice(-1 * cnt);

export const argsSplit = <T>(args: Array<T>, cnt: number): [ Array<T>, Array<T> ] =>
  cnt == 0 ? [args, []] : [ args.slice(0, args.length - cnt), args.slice(-1 * cnt) ];

export const objectMap = <A,B>(object: {[key:string]: A}, mapFn: ((k:string, a:A) => B)): {[key:string]: B} =>
  Object.keys(object).reduce(function(result: {[key:string]: B}, key:string) {
    result[key] = mapFn(key, object[key])
    return result;
  }, {});

export const mkAddressEq = (T_Address: {canonicalize: (addr:any) => any}
) => (x:any, y:any): boolean =>
  bytesEq(T_Address.canonicalize(x), T_Address.canonicalize(y));
