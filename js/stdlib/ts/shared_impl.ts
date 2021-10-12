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
  _Participants: {[n: string]: any},
  _APIs: {[n: string]: {[n: string]: any}},
};

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
  didSend: boolean,
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
  lct: BigNumber,
  sim_p: (fake: IRecv<RawAddress>) => Promise<ISimRes<Token>>,
};

export type IRecvArgs<ConnectorTy extends AnyBackendTy> = {
  funcNum: number,
  evt_cnt: number,
  out_tys: Array<ConnectorTy>,
  didSend: boolean,
  waitIfNotPresent: boolean,
  timeoutAt: TimeArg | undefined,
};

export type ParticipantVal = (io:any) => Promise<any>;
export type ParticipantMap = {[key: string]: ParticipantVal};
export type ViewVal = (...args:any) => Promise<any>;
export type ViewFunMap = {[key: string]: ViewVal};
export type ViewMap = {[key: string]: ViewFunMap};
export type APIMap = ViewMap;

export type IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  getContractAddress: () => Promise<CBR_Address>,
  waitUntilTime: (v:BigNumber) => Promise<BigNumber>,
  waitUntilSecs: (v:BigNumber) => Promise<BigNumber>,
  selfAddress: () => CBR_Address, // Not RawAddress!
  iam: (some_addr: RawAddress) => RawAddress,
  stdlib: Object,
  sendrecv: (args:ISendRecvArgs<RawAddress, Token, ConnectorTy>) => Promise<IRecv<RawAddress>>,
  recv: (args:IRecvArgs<ConnectorTy>) => Promise<IRecv<RawAddress>>,
};

export type ISetupArgs<ContractInfo> = {
  setInfo: (info: ContractInfo) => void,
  getInfo: () => Promise<ContractInfo>,
};
export type ISetupRes<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = Pick<IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>, ("getContractAddress"|"sendrecv"|"recv")>;

export type IStdContractArgs<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  bin: IBackend<ConnectorTy>,
  setupView: ISetupView<ContractInfo, ConnectorTy>,
  givenInfoP: (Promise<ContractInfo>|undefined)
  _setup: (args: ISetupArgs<ContractInfo>) => ISetupRes<ContractInfo, RawAddress, Token, ConnectorTy>,
} & Omit<IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>, ("getInfo"|"getContractAddress"|"sendrecv"|"recv")>;

export type IContract<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  getViews: () => ViewMap,
  getContractAddress: () => Promise<CBR_Address>,
  // backend-specific
  participants: ParticipantMap,
  p: ParticipantMap
  views: ViewMap,
  v: ViewMap,
  apis: APIMap,
  a: APIMap,
  // for compiled output
  _initialize: () => IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>,
};

export type ISetupView<ContractInfo, ConnectorTy extends AnyBackendTy> = (getInfo:(() => Promise<ContractInfo>)) => {
  viewLib: IViewLib,
  getView1: ((views:IBackendViewsInfo<ConnectorTy>, v:string, k:string, vi:IBackendViewInfo<ConnectorTy>) => ViewVal)
};

export const stdContract =
  <ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy>(
    stdContractArgs: IStdContractArgs<ContractInfo, RawAddress, Token, ConnectorTy>):
  IContract<ContractInfo, RawAddress, Token, ConnectorTy> => {
  const { bin, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, _setup, givenInfoP } = stdContractArgs;

  const { setInfo, getInfo }: ISetupArgs<ContractInfo> = (() => {
    let _setInfo = (info:ContractInfo) => {
      throw Error(`Cannot set info(${JSON.stringify(info)}) when acc.contract called with contract info`);
      return;
    };
    if ( givenInfoP !== undefined ) {
      return {
        setInfo: _setInfo,
        getInfo: (() => givenInfoP),
      };
    } else {
      let beenSet = false;
      const _infoP: Promise<ContractInfo> = new Promise((resolve) => {
        _setInfo = (info:ContractInfo) => {
          if ( beenSet ) {
            throw Error(`Cannot set info(${JSON.stringify(info)}) twice`);
          }
          resolve(info);
          beenSet = true;
        };
      });
      return {
        setInfo: _setInfo,
        getInfo: (() => _infoP),
      };
    }
  })();

  const _initialize = () => {
    const { getContractAddress, sendrecv, recv } = _setup({ setInfo, getInfo });
    return {
      selfAddress, iam, stdlib, waitUntilTime, waitUntilSecs,
      getInfo,
      getContractAddress, sendrecv, recv,
    };
  };
  const ctcC = { _initialize };

  const { viewLib, getView1 } = setupView(getInfo);
  const views_bin = bin._getViews({reachStdlib: stdlib}, viewLib);
  const views =
    objectMap(views_bin.infos, ((v:string, vm:{[keyn:string]: IBackendViewInfo<ConnectorTy>}) =>
      objectMap(vm, ((k:string, vi:IBackendViewInfo<ConnectorTy>) =>
        getView1(views_bin.views, v, k, vi)))));

  const participants = objectMap(bin._Participants, ((pn:string, p:any) => {
      void(pn);
      return ((io:any) => {
        return p(ctcC, io);
      });
  }));

  const apis = objectMap(bin._APIs, ((an:string, am:any) => {
    return objectMap(am, ((afn:string, ab:any) => {
      const bl = `${an}_${afn}`;
      return (...args:any[]) => {
        let theResolve: (x:any) => void;
        const p = new Promise((resolve) => {
          theResolve = resolve;
        });
        ab(ctcC, {
          "in": (() => {
            debug(`${bl}: in`, args);
            return args
          }),
          "out": ((oargs:any[], res:any) => {
            console.log(`${bl}: out`, oargs, res);
            theResolve(res);
            return new Promise((res, rej) => (void(res), rej('fail')));
          }),
        }).catch((err:any) => {
          console.log(`${bl}: done`, err);
        });
        return p;
      };
    }));
  }));

  return {
    ...ctcC,
    getInfo,
    getContractAddress: (() => _initialize().getContractAddress()),
    participants, p: participants,
    views, v: views,
    getViews: () => {
      console.log(`WARNING: ctc.getViews() is deprecated; use ctc.views or ctc.v instead.`);
      return views;
    },
    apis, a: apis,
  };
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token> = {
  networkAccount: NetworkAccount,
  deploy: (bin: Backend) => Contract,
  attach: (bin: Backend, ctcInfoP: Promise<ContractInfo>) => Contract,
  contract: (bin: Backend, ctcInfoP?: Promise<ContractInfo>) => Contract,
  stdlib: Object,
  getAddress: () => string,
  setDebugLabel: (lab: string) => IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>,
  tokenAccept: (token: Token) => Promise<void>,
  tokenMetadata: (token: Token) => Promise<any>,
};

export const stdAccount =
  <NetworkAccount, Backend, Contract, ContractInfo, Token>(
    orig:Omit<IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>, ("deploy"|"attach")>):
  IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token> => {
  return {
    ...orig,
    deploy: (bin: Backend) => {
      console.log(`WARNING: acc.deploy(bin) is deprecated; use acc.contract(bin) instead. Deployment is implied by the first publication.`);
      return orig.contract(bin, undefined);
    },
    attach: (bin: Backend, ctcInfoP: Promise<ContractInfo>) => {
      console.log(`WARNING: acc.attach(bin, info) is deprecated; use acc.contract(bin, info) instead. Attachment is implied by reception of the first publication.`);
      return orig.contract(bin, ctcInfoP);
    },
  };
};

export type IAccountTransferable<NetworkAccount> = IAccount<NetworkAccount, any, any, any, any> | {
  networkAccount: NetworkAccount,
}

export type ISimRes<Token> = {
  txns: Array<ISimTxn<Token>>,
  mapRefs: Array<string>,
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

/** @description Check that a stringy env value doesn't look falsy. */
export function truthyEnv(v: string|undefined|null): v is string {
  if (!v) return false;
  return ![
    '0', 'false', 'f', '#f', 'no', 'off', 'n', '',
  ].includes(v && v.toLowerCase && v.toLowerCase());
}

export const envDefault = <T>(v: string|undefined|null, d: T): string|T =>
  (v === undefined || v === null) ? d : v;

export const envDefaultNoEmpty = <T>(v: string|undefined|null, d: T): string|T => {
  const v2 = envDefault(v, d);
  return v2 === '' ? d : v2;
}

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
  const muldiv = (a: num, b: num, c: num): BigNumber => {
    const prod = bigNumberify(a).mul(bigNumberify(b));
    return check( prod.div(bigNumberify(c)) );
  };
  return { add, sub, mod, mul, div, muldiv };
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
    Promise.all((new Array(k)).fill(1).map((_:any): Promise<X> => newTestAccount(bal)));

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
  debug('checkTimeout', { timeoutAt, nowTimeN });
  if ( ! timeoutAt ) { return false; }
  const [ mode, val ] = timeoutAt;
  const nowTime = bigNumberify(nowTimeN);
  if ( mode === 'time' ) {
    return val.lte(nowTime);
  } else if ( mode === 'secs' ) {
    const nowSecs = await getTimeSecs(nowTime);
    return val.lte(nowSecs);
  } else {
    throw new Error(`invalid TimeArg mode`);
  }
};

export class Signal {
  p: Promise<boolean>;
  r: (a:boolean) => void;

  constructor() {
    this.r = (a) => { void(a); throw new Error(`signal never initialized`); };
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

// Given a func that takes an optional arg, and a Maybe arg:
// f: (arg?: X) => Y
// arg: Maybe<X>
//
// You can apply the function like this:
// f(...argMay)
export type Some<T> = [T];
export type None = [];
export type Maybe<T> = None | Some<T>;
export function isNone<T>(m: Maybe<T>): m is None {
  return m.length === 0;
}
export function isSome<T>(m: Maybe<T>): m is Some<T> {
  return !isNone(m);
}
export const Some = <T>(m: T): Some<T> => [m];
export const None: None = [];
