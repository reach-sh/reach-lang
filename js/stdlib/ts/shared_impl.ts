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

export const bigNumberToBigInt = (x:BigNumber): bigint => BigInt(x.toHexString());

type BigNumber = ethers.BigNumber;

export type CurrencyAmount = string | number | BigNumber | bigint

export type {Connector} from './ConnectorMode';

let DEBUG: boolean = truthyEnv(process.env.REACH_DEBUG);

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
  decode: (i:number, svs:Array<any>, args:Array<any>) => Promise<any>,
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

export type IViewLib = {
  viewMapRef: any,
};

export type IBackend<ConnectorTy extends AnyBackendTy> = {
  _backendVersion: number,
  _getViews: (stdlib:Object, viewlib:IViewLib) => IBackendViews<ConnectorTy>,
  _getMaps: (stdlib:Object) => IBackendMaps<ConnectorTy>,
};

export const getViewsHelper =
  <ConnectorTy extends AnyBackendTy, B>(views:IBackendViews<ConnectorTy>, getView1:((views:IBackendViewsInfo<ConnectorTy>, v:string, k:string, vi:IBackendViewInfo<ConnectorTy>) => B)) =>
    () =>
      objectMap(views.infos, ((v:string, vm:{[keyn:string]: IBackendViewInfo<ConnectorTy>}) =>
        objectMap(vm, ((k:string, vi:IBackendViewInfo<ConnectorTy>) =>
            getView1(views.views, v, k, vi)))));

export type OnProgress = (obj: {current: BigNumber, target: BigNumber}) => any;

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
  secs: BigNumber,
  getOutput: (o_mode:string, o_lab:string, o_ctc:any) => Promise<any>,
};

export type IRecv<RawAddress> = IRecvNoTimeout<RawAddress> | {
  didTimeout: true,
};

export type TimeArg = [ ('time' | 'secs'), BigNumber ];

export type ISendRecvArgs<RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  funcNum: number,
  evt_cnt: number,
  tys: Array<ConnectorTy>,
  args: Array<any>,
  pay: MkPayAmt<Token>,
  out_tys: Array<ConnectorTy>,
  onlyIf: boolean,
  soloSend: boolean,
  timeoutAt: TimeArg | undefined,
  sim_p: (fake: IRecv<RawAddress>) => Promise<ISimRes<Token, ConnectorTy>>,
};

export type IRecvArgs<ConnectorTy extends AnyBackendTy> = {
  funcNum: number, evt_cnt: number, out_tys: Array<ConnectorTy>,
  waitIfNotPresent: boolean,
  timeoutAt: TimeArg | undefined,
};

export type IContract<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  creationTime: () => Promise<BigNumber>,
  creationSecs: () => Promise<BigNumber>,
  sendrecv: (args:ISendRecvArgs<RawAddress, Token, ConnectorTy>) => Promise<IRecv<RawAddress>>,
  recv: (args:IRecvArgs<ConnectorTy>) => Promise<IRecv<RawAddress>>,
  waitTime: (v:BigNumber) => Promise<BigNumber>,
  waitSecs: (v:BigNumber) => Promise<BigNumber>,
  iam: (some_addr: RawAddress) => RawAddress,
  selfAddress: () => CBR_Address, // Not RawAddress!
  getViews: () => {[key: string]: {[key: string]: (() => Promise<any>)}},
  stdlib: Object,
};

type ContractIndex = 'getInfo' | 'creationTime' | 'creationSecs' | 'sendrecv' | 'recv' | 'waitTime' | 'waitSecs' | 'iam' | 'selfAddress' | 'getViews' | 'stdlib';

export const deferContract =
  <ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy>(
    shouldError: boolean,
    implP:Promise<IContract<ContractInfo, RawAddress, Token, ConnectorTy>>,
    implNow: Partial<IContract<ContractInfo, RawAddress, Token, ConnectorTy>>):
  IContract<ContractInfo, RawAddress, Token, ConnectorTy> => {

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
  let impl: IContract<ContractInfo, RawAddress, Token, ConnectorTy> = {
    getInfo: delay('getInfo'),
    // @ts-ignore
    creationTime: delay('creationTime'),
    // @ts-ignore
    creationSecs: delay('creationSecs'),
    // @ts-ignore
    sendrecv: mnow('sendrecv'),
    // @ts-ignore
    recv: mnow('recv'),
    // @ts-ignore
    waitTime: not_yet('waitTime'),
    // @ts-ignore
    waitSecs: not_yet('waitSecs'),
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
    waitTime: wrap('waitTime'),
    waitSecs: wrap('waitSecs'),
    getInfo: wrap('getInfo'),
    creationTime: wrap('creationTime'),
    creationSecs: wrap('creationSecs'),
    iam: wrap('iam'),
    selfAddress: wrap('selfAddress'),
    getViews: wrap('getViews'),
    stdlib: impl.stdlib,
  };
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token> = {
  networkAccount: NetworkAccount,
  deploy: (bin: Backend) => Contract,
  attach: (bin: Backend, ctcInfoP: Promise<ContractInfo>) => Contract,
  stdlib: Object,
  getAddress: () => string,
  setDebugLabel: (lab: string) => IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>,
  tokenAccept: (token: Token) => Promise<void>,
  tokenMetadata: (token: Token) => Promise<any>,
}

export type IAccountTransferable<NetworkAccount> = IAccount<NetworkAccount, any, any, any, any> | {
  networkAccount: NetworkAccount,
}

export type ISimRes<Token, ConnectorTy> = {
  txns: Array<ISimTxn<Token>>,
  mapRefs: Array<string>,
  mapsPrev: any,
  mapsNext: any,
  view: [ ConnectorTy, any ],
  isHalt : boolean,
};

export type ISimTxn<Token> = {
  kind: 'to'|'init',
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: 'from',
  to: string,
  amt: BigNumber,
  tok: Token|undefined,
} | {
  kind: 'halt',
  tok: Token|undefined,
} | {
  kind: 'tokenNew',
  n: any,
  s: any,
  u: any
  m: any,
  p: BigNumber,
} | {
  kind: 'tokenBurn',
  tok: Token,
  amt: BigNumber,
} | {
  kind: 'tokenDestroy',
  tok: Token,
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

/** @description Access an environment variable, or its react-prefixed equivalent */
export function rEnv(env: {[k: string]: string}, k: string): string|undefined {
  return env[k] || env[`REACT_APP_${k}`];
}

/** @description Check that a stringy env value doesn't look falsy. */
export function truthyEnv(v: string|undefined|null): v is string {
  if (!v) return false;
  return ![
    '0', 'false', 'f', '#f', 'no', 'off',
  ].includes(v && v.toLowerCase && v.toLowerCase());
}

export const envDefault = <T>(v: string|undefined|null, d: T): string|T =>
  (v === undefined || v === null) ? d : v;

type DigestMode = 'keccak256' | 'sha256';
export const makeDigest = (mode: DigestMode, prep: any) => (t:any, v:any) => {
  void(hexlify);
  // const args = [t, v];
  // debug('digest(', args, ') =>');
  const kekCat = prep(t, v);
  // debug('digest(', args, ') => internal(', hexlify(kekCat), ')');
  const f = mode === 'keccak256' ? ethers.utils.keccak256 : ethers.utils.sha256;
  const r = f(kekCat);
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

export const ensureConnectorAvailable = (bin:any, conn: string, jsVer: number, connVer: number) => {
  checkVersion(bin._backendVersion, jsVer, `JavaScript backend`);
  const connectors = bin._Connectors;
  const conn_bin = connectors[conn];
  if ( ! conn_bin ) {
    throw (new Error(`The application was not compiled for the ${conn} connector, only: ${Object.keys(connectors)}`));
  }
  checkVersion(conn_bin.version, connVer, `${conn} backend`);
};

export const checkVersion = (actual:number, expected:number, label:string): void => {
  if ( actual !== expected ) {
    const older = (actual === undefined) || (actual < expected);
    const more = older ? `update your compiler and recompile!` : `updated your standard library and rerun!`;
    throw Error(`This Reach compiled ${label} does not match the expectations of this Reach standard library: expected ${expected}, but got ${actual}; ${more}`);
  }
};

const argHelper = (xs: any[], f: (_:any) => any, op: (a: any, b: any) => boolean) => {
  if (xs.length == 0) {
    return undefined;
  }
  return xs.reduce((accum: any, x: any) =>
    op(f(x), f(accum)) ? x : accum, xs[0]);
}

export const argMax = (xs: any[], f: (_:any) => any) =>
  argHelper(xs, f, (a, b) => a > b);

export const argMin = (xs: any[], f: (_:any) => any) =>
  argHelper(xs, f, (a, b) => a < b);

export const make_newTestAccounts = <X>(newTestAccount: (bal:any) => Promise<X>): ((k:number, bal:any) => Promise<Array<X>>) =>
  (k:number, bal:any): Promise<Array<X>> =>
    Promise.all((new Array(k)).map((_:any): Promise<X> => newTestAccount(bal)));

export const make_waitUntilX = (label: string, getCurrent: () => Promise<BigNumber>, step: (target:BigNumber) => Promise<BigNumber>) => async (target: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg = onProgress || (() => {});
  let current = await getCurrent();
  const notify = () => {
    const o = { current, target };
    debug(`waitUntilX:`, label, o);
    onProg(o);
  };
  while (current.lt(target)) {
    current = await step(current.add(1));
    notify();
  }
  notify();
  return current;
};

export const checkTimeout = async (getTimeSecs: ((now:BigNumber) => Promise<BigNumber>), timeoutAt: TimeArg | undefined, nowTimeN: number): Promise<boolean> => {
  if ( ! timeoutAt ) { return false; }
  const [ mode, val ] = timeoutAt;
  const nowTime = bigNumberify(nowTimeN);
  if ( mode === 'time' ) {
    return val.lt(nowTime);
  } else if ( mode === 'secs' ) {
    const nowSecs = await getTimeSecs(nowTime);
    return val.lt(nowSecs);
  } else {
    throw new Error(`invalid TimeArg mode`);
  }
};

