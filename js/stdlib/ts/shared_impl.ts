// This can depend on the shared backend
import crypto from 'crypto';
import Timeout from 'await-timeout';
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
  assert,
  formatAssertInfo,
} from './shared_backend';
import type { MapRefT } from './shared_backend'; // =>
import { process } from './shim';
export {
  hexlify
} from './shared_backend';
import type { Arith } from './interfaces';

type BigNumber = ethers.BigNumber;

export type CurrencyAmount = string | number | BigNumber | bigint

export type {Connector} from './ConnectorMode';

export const j2sf = (x:any): string => {
  // We're removing duplicated values, so we can remove cyclic references
  const seen: any[] = [];
  const rep = (key:any, val:any): any => {
    void key;
    if (val != null && typeof val === "object") {
      const idx = seen.indexOf(val);
      if ( idx >= 0) { return `@${idx}`; }
      seen.push(val);
    }
    return val;
  };
  return JSON.stringify(x, rep, 2);
};

export const j2s = (x:any): string => {
  let xs = `${x}`;
  if ( ! (x && x._isBigNumber) && (xs === '{}' || xs.startsWith('[object')) ) {
    return j2sf(x);
  }
  return xs;
};

let DEBUG: boolean = truthyEnv(process.env.REACH_DEBUG);

export const setDEBUG = (b: boolean) => {
  if (b === false || b === true) {
    DEBUG = b;
  } else {
    throw Error(`Expected bool, got ${j2s(b)}`);
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

const isUntaggedView = (x: any) => {
  return 'ty' in x && 'decode' in x;
}

export type IBackendViewsInfo<ConnectorTy extends AnyBackendTy> =
  {[viewi: number]: Array<ConnectorTy>};

export type TaggedBackendView<ConnectorTy extends AnyBackendTy> =
  {[keyn:string]: IBackendViewInfo<ConnectorTy>}

export type IBackendViews<ConnectorTy extends AnyBackendTy> = {
  views: IBackendViewsInfo<ConnectorTy>,
  infos: {
    [viewn: string]: TaggedBackendView<ConnectorTy> | IBackendViewInfo<ConnectorTy>,
  },
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
  _APIs: {[n: string]: any | {[n: string]: any}},
  _stateSourceMap: {[key: number]: any},
  _getEvents: (stdlib:Object) => ({ [n:string]: [any] })
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
  getOutput: (o_mode:string, o_lab:string, o_ctc:any, o_val:any) => Promise<any>,
};

export type IRecv<RawAddress> = IRecvNoTimeout<RawAddress> | {
  didTimeout: true,
};

export type TimeArg = [ ('time' | 'secs'), BigNumber ];

export type ISendRecvArgs<RawAddress, Token, ConnectorTy extends AnyBackendTy, ContractInfo> = {
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
  sim_p: (fake: IRecv<RawAddress>) => Promise<ISimRes<Token, ContractInfo>>,
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
export type ViewMap = {[key: string]: ViewVal | ViewFunMap};
export type APIMap = ViewMap;
export type EventMap = { [key: string]: any }

export type IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getContractInfo: () => Promise<ContractInfo>,
  getContractAddress: () => Promise<CBR_Address>,
  getBalance: () => Promise<BigNumber>,
  waitUntilTime: (v:BigNumber) => Promise<BigNumber>,
  waitUntilSecs: (v:BigNumber) => Promise<BigNumber>,
  selfAddress: () => CBR_Address, // Not RawAddress!
  iam: (some_addr: RawAddress) => RawAddress,
  stdlib: Object,
  sendrecv: (args:ISendRecvArgs<RawAddress, Token, ConnectorTy, ContractInfo>) => Promise<IRecv<RawAddress>>,
  recv: (args:IRecvArgs<ConnectorTy>) => Promise<IRecv<RawAddress>>,
  getState: (v:BigNumber, ctcs:Array<ConnectorTy>) => Promise<Array<any>>,
  getCurrentStep: () => Promise<BigNumber>,
  apiMapRef: (i:number, ty:ConnectorTy) => MapRefT<any>,
};

export type ISetupArgs<ContractInfo, VerifyResult> = {
  setInfo: (info: ContractInfo) => void,
  getInfo: () => Promise<ContractInfo>,
  setTrustedVerifyResult: (vr:VerifyResult) => void,
  getTrustedVerifyResult: () => (VerifyResult|undefined),
};
export type ISetupViewArgs<ContractInfo, VerifyResult> =
  Omit<ISetupArgs<ContractInfo, VerifyResult>, ("setInfo")>;

export type ISetupEventArgs<ContractInfo, VerifyResult> =
  Omit<ISetupArgs<ContractInfo, VerifyResult>, ("setInfo")>;

type SpecificKeys = ("getContractInfo"|"getContractAddress"|"getBalance"|"sendrecv"|"recv"|"getState"|"getCurrentStep"|"apiMapRef");

export type ISetupRes<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = Pick<IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>, (SpecificKeys)>;

export type IStdContractArgs<ContractInfo, VerifyResult, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  bin: IBackend<ConnectorTy>,
  getABI: (x?:boolean) => unknown,
  setupView: ISetupView<ContractInfo, VerifyResult, ConnectorTy>,
  setupEvents: ISetupEvent<ContractInfo, VerifyResult>,
  givenInfoP: (Promise<ContractInfo>|undefined)
  _setup: (args: ISetupArgs<ContractInfo, VerifyResult>) => ISetupRes<ContractInfo, RawAddress, Token, ConnectorTy>,
} & Omit<IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>, (SpecificKeys)>;

export type IContract<ContractInfo, RawAddress, Token, ConnectorTy extends AnyBackendTy> = {
  getInfo: () => Promise<ContractInfo>,
  getViews: () => ViewMap,
  getContractAddress: () => Promise<CBR_Address>,
  // backend-specific
  getABI: (x?:boolean) => unknown,
  participants: ParticipantMap,
  p: ParticipantMap
  views: ViewMap,
  v: ViewMap,
  unsafeViews: ViewMap,
  apis: APIMap,
  a: APIMap,
  safeApis: APIMap,
  e: EventMap,
  events: EventMap,
  // for compiled output
  _initialize: () => IContractCompiled<ContractInfo, RawAddress, Token, ConnectorTy>,
};

export type ISetupView<ContractInfo, VerifyResult, ConnectorTy extends AnyBackendTy> = (args:ISetupViewArgs<ContractInfo, VerifyResult>) => {
  viewLib: IViewLib,
  getView1: ((views:IBackendViewsInfo<ConnectorTy>, v:string, k:string|undefined, vi:IBackendViewInfo<ConnectorTy>, isSafe: boolean) => ViewVal)
};

export type ISetupEvent<ContractInfo, VerifyResult> =
  (args:ISetupEventArgs<ContractInfo, VerifyResult>) => {
      createEventStream : (event: string, tys: any[]) => {
                            lastTime: () => Promise<Time>,
                            next: () => Promise<any>,
                            seek: (t: Time) => void,
                            seekNow: () => Promise<void>,
                            monitor: (onEvent: (x:any) => void) => Promise<void>
                          }
    }

export type Time = BigNumber;

export type Event<T> = {
  when: Time,
  what: T
}

export type EventStream<T> = {
  // mvp
  seek: (t:Time) => void,
  next: () => Promise<Event<T>>
  // additional
  seekNow: () => void,
  lastTime: () => Time,
  // why can't TS handle a function type as arg
  monitor: (f: any) => void
}

export const stdVerifyContract =
  async <ContractInfo, VerifyResult>(
    stdArgs: Pick<ISetupViewArgs<ContractInfo, VerifyResult>, ("getTrustedVerifyResult"|"setTrustedVerifyResult")>,
    doVerify: (() => Promise<VerifyResult>)
  ): Promise<VerifyResult> => {
    const { getTrustedVerifyResult, setTrustedVerifyResult } = stdArgs;
    let r = getTrustedVerifyResult();
    if ( r ) { return r; }
    r = await doVerify();
    setTrustedVerifyResult(r);
    return r;
  };

export const stdABIFilter = (x:any) => {
  if ( x.name && x.name.startsWith('_reach') ) { return false; }
  return true;
};
export const stdGetABI = (ABI:any) => (isFull?:boolean) =>
  isFull ? ABI : ABI.filter(stdABIFilter);

export const stdContract =
  <ContractInfo, VerifyResult, RawAddress, Token, ConnectorTy extends AnyBackendTy>(
    stdContractArgs: IStdContractArgs<ContractInfo, VerifyResult, RawAddress, Token, ConnectorTy>):
  IContract<ContractInfo, RawAddress, Token, ConnectorTy> => {
  const { bin, getABI, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, setupEvents, _setup, givenInfoP } = stdContractArgs;

  type SomeSetupArgs = Pick<ISetupArgs<ContractInfo, VerifyResult>, ("setInfo"|"getInfo")>;
  const { setInfo, getInfo }: SomeSetupArgs = (() => {
    let _setInfo = (info:ContractInfo) => {
      throw Error(`Cannot set info(${j2s(info)}) (i.e. deploy) when acc.contract called with contract info: You are trying to attach to a contract as the deployer, which is not possible.`);
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
            throw Error(`Cannot set info(${j2s(info)}) (i.e. deploy) twice`);
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

  let trustedVerifyResult:any = undefined;
  const getTrustedVerifyResult = () => trustedVerifyResult;
  const setTrustedVerifyResult = (x:any) => { trustedVerifyResult = x; };

  const viewArgs = { getInfo, setTrustedVerifyResult, getTrustedVerifyResult };
  const setupArgs = { ...viewArgs, setInfo };

  const _initialize = () => {
    const { getContractInfo, getContractAddress, getBalance,
            sendrecv, recv, getCurrentStep, getState, apiMapRef } =
      _setup(setupArgs);
    return {
      selfAddress, iam, stdlib, waitUntilTime, waitUntilSecs,
      getContractInfo, getContractAddress, getBalance,
      sendrecv, recv,
      getCurrentStep, getState, apiMapRef,
    };
  };
  const ctcC = { _initialize };

  const { viewLib, getView1 } = setupView(viewArgs);
  const views_bin = bin._getViews({reachStdlib: stdlib}, viewLib);
  const mkViews = (isSafe: boolean) =>
    objectMap(views_bin.infos, ((v:string, vm: TaggedBackendView<ConnectorTy> | IBackendViewInfo<ConnectorTy>) =>
      isUntaggedView(vm)
        ? getView1(views_bin.views, v, undefined, vm as IBackendViewInfo<ConnectorTy>, isSafe)
        : objectMap(vm as TaggedBackendView<ConnectorTy>, ((k:string, vi:IBackendViewInfo<ConnectorTy>) =>
            getView1(views_bin.views, v, k, vi, isSafe)))));

  const views = mkViews(true);
  const unsafeViews = mkViews(false);

  const participants = objectMap(bin._Participants, ((pn:string, p:any) => {
      void(pn);
      return ((io:any) => {
        return p(ctcC, io);
      });
  }));

  const mkApis = (isSafe = false) =>
    objectMap(bin._APIs, ((an:string, am:any) => {
      const f = (afn:string|undefined, ab:any) => {
        const mk = (sep: string) =>
          (afn === undefined) ? `${an}` : `${an}${sep}${afn}`;
        const bp = mk(`_`);
        delete participants[bp];
        const bl = mk(`.`);
        return (...args:any[]) => {
          const terminal = { terminated: bl };
          let theResolve: (x:any) => void;
          let theReject: (x:any) => void;
          const p = new Promise((resolve, reject) => {
            theResolve = resolve;
            theReject = reject;
          });
          const fail = (err: Error) => {
            if (isSafe) {
              theResolve(['None', null]);
            } else {
              theReject(err);
            }
          };
          debug(`${bl}: start`, args);
          ab(ctcC, {
            "in": (() => {
              debug(`${bl}: in`, args);
              return args
            }),
            "out": ((oargs:any[], res:any) => {
              debug(`${bl}: out`, oargs, res);
              theResolve(isSafe ? ['Some', res] : res);
              throw terminal;
            }),
          }).catch((err:any) => {
            if ( Object.is(err, terminal) ) {
              debug(`${bl}: done`);
            } else {
              fail(new Error(`${bl} errored with ${err}`));
            }
          }).then((res:any) => {
            fail(new Error(`${bl} returned with ${j2s(res)}`));
          });
          return p;
        };
      };
      return (typeof am === 'object')
        ? objectMap(am, f)
        : f(undefined, am);
  }));

  const apis = mkApis(false);
  const safeApis = mkApis(true);

  const eventMap = bin._getEvents({ reachStdlib: stdlib });
  const { createEventStream } = setupEvents(viewArgs);

  const events =
    objectMap(eventMap, ((k:string, v: any) =>
      Array.isArray(v) // untagged
        ? createEventStream(k, v)
        : objectMap(v, ((kp, vp: any) =>
          createEventStream(k + "_" + kp, vp)))));

  return {
    ...ctcC,
    getABI,
    getInfo,
    getContractAddress: (() => _initialize().getContractAddress()),
    participants, p: participants,
    views, v: views,
    getViews: () => {
      console.log(`WARNING: ctc.getViews() is deprecated; use ctc.views or ctc.v instead.`);
      return views;
    },
    unsafeViews,
    apis, a: apis,
    safeApis,
    events, e: events,
  };
};

export type TokenMetadata = {
  name?: string,
  symbol?: string,
  url?: string,
  metadata?: string,
  supply: BigNumber,
  decimals?: BigNumber,
};

export type LaunchTokenOpts = {
  'decimals'?: number,
  'supply'?: unknown,
  'clawback'?: any,
  'url'?: string,
  'metadataHash'?: string,
  'note'?: Uint8Array,
};

export type IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token> = {
  networkAccount: NetworkAccount,
  /**
   * @deprecated Use
   * [`contract`](https://docs.reach.sh/frontend/#js_contract)
   * instead.
   */
  deploy: (bin: Backend) => Contract,
  /**
   * @deprecated Use
   * [`contract`](https://docs.reach.sh/frontend/#js_contract)
   * instead.
   */
  attach: (bin: Backend, ctcInfoP: Promise<ContractInfo>) => Contract,
  contract: (bin: Backend, ctcInfoP?: Promise<ContractInfo>) => Contract,
  stdlib: Object,
  getAddress: () => string,
  setDebugLabel: (lab: string) => IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>,
  tokenAccept: (token: Token) => Promise<void>,
  tokenAccepted: (token: Token) => Promise<boolean>,
  tokenMetadata: (token: Token) => Promise<TokenMetadata>,
  setGasLimit: (ngl:unknown) => void,
  getGasLimit: () => BigNumber,
  setStorageLimit: (nsl:unknown) => void,
  getStorageLimit: () => BigNumber,
};

export const stdAccount_unsupported =
  <NetworkAccount, Backend, Contract, ContractInfo, Token>(
    conn:string):
  Pick<IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>, ("setGasLimit"|"getGasLimit"|"setStorageLimit"|"getStorageLimit")> => {
  const setGasLimit = (ngl:unknown): void => {
    void(ngl);
    console.warn(`setGasLimit not supported on ${conn}`);
  };
  const getGasLimit = (): BigNumber => {
    console.warn(`getGasLimit not supported on ${conn}`);
    return bigNumberify(0);
  };
  const setStorageLimit = (ngl:unknown): void => {
    void(ngl);
    console.warn(`setStorageLimit not supported on ${conn}`);
  };
  const getStorageLimit = (): BigNumber => {
    console.warn(`getStorageLimit not supported on ${conn}`);
    return bigNumberify(0);
  };
  return { setGasLimit, getGasLimit, setStorageLimit, getStorageLimit };
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

export type ISimRes<Token, ContractInfo> = {
  txns: Array<ISimTxn<Token, ContractInfo>>,
  mapRefs: Array<string>,
  isHalt : boolean,
};

// XXX Add Address
export type ISimTxn<Token, ContractInfo> = {
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
  d: BigNumber|undefined,
} | {
  kind: 'tokenBurn',
  tok: Token,
  amt: BigNumber,
} | {
  kind: 'tokenDestroy',
  tok: Token,
} | {
  kind: 'remote',
  obj: ContractInfo,
  pays: BigNumber,
  bills: BigNumber,
  toks: Array<Token>,
  accs: Array<string>,
  fees: BigNumber,
} | {
  kind: 'info',
  tok: Token,
} | {
  kind: 'api',
  who: string,
} ;

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
  debug('digest', {mode, prep, t, v, kekCat, f, r});
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

export type UIntTy = boolean;
export const UInt256_max =
  ethers.BigNumber.from(2).pow(256).sub(1);
export const makeArith = (m:BigNumber): Arith => {
  const checkB = (x: BigNumber) =>
    checkedBigNumberify(`internal`, UInt256_max, x);
  const checkM = (x: BigNumber) =>
    checkedBigNumberify(`internal`, m, x);

  type BNOp = 'add'|'sub'|'mod'|'mul'|'div'|'and'|'or'|'xor';
  const doBN = (f:BNOp, a:BigNumber, b:BigNumber) => a[f](b);
  const getCheck = (w:UIntTy) => w ? checkB : checkM;
  const cast = (from:UIntTy, to:UIntTy, x:num): BigNumber => {
    const checkF = getCheck(from);
    const checkT = getCheck(to);
    return checkT(checkF(bigNumberify(x)));
  };

  const liftX = (check:(x:BigNumber) => BigNumber) => (f:BNOp) => (a:num, b:num): BigNumber => check(doBN(f, bigNumberify(a), bigNumberify(b)));
  const liftB = liftX(checkB);
  const liftM = liftX(checkM);
  const add = liftM('add');
  const sub = liftM('sub');
  const mod = liftM('mod');
  const mul = liftM('mul');
  const div = liftM('div');
  const band = liftM('and');
  const bior = liftM('or');
  const bxor = liftM('xor');
  const add256 = liftB('add');
  const sub256 = liftB('sub');
  const mod256 = liftB('mod');
  const mul256 = liftB('mul');
  const div256 = liftB('div');
  const band256 = liftB('and');
  const bior256 = liftB('or');
  const bxor256 = liftB('xor');
  const muldiv = (a: num, b: num, c: num): BigNumber => {
    const prod = bigNumberify(a).mul(bigNumberify(b));
    return checkM( prod.div(bigNumberify(c)) );
  };
  return {
    add, sub, mod, mul, div, band, bior, bxor,
    add256, sub256, mod256, mul256, div256, band256, bior256, bxor256,
    cast, muldiv };
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
    const more = older ? `update your compiler and recompile!` : `update your standard library and rerun!`;
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

type NewTestAccounts<X> = (k:number, bal:any) => Promise<Array<X>>;
export const make_newTestAccounts = <X>(newTestAccount: (bal:any) => Promise<X>): {
  parallel: NewTestAccounts<X>,
  serial: NewTestAccounts<X>
} => {
  const makeArr = (k:number): Array<number> => (new Array(k)).fill(1);
  const parallel: NewTestAccounts<X> = (k, bal) =>
    Promise.all(makeArr(k).map((_:any): Promise<X> => newTestAccount(bal)));
  const serial: NewTestAccounts<X> = async (k, bal) => {
    const arr = [];
    for ( let i = 0; i < k; i++ ) {
      arr.push(await newTestAccount(bal));
    }
    return arr;
  };
  return { parallel, serial };
};

export const make_waitUntilX = (label: string, getCurrent: () => Promise<BigNumber>, step: (target:BigNumber) => Promise<BigNumber>) => async (target: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg = onProgress || (() => {});
  let current = await getCurrent();
  const notify = () => {
    const o = { current, target };
    debug(`waitUntilX:`, label, o);
    onProg(o);
  };
  while (current.lt(target)) {
    debug('waitUntilX', { label, current, target });
    current = await step(current.add(1));
    notify();
  }
  notify();
  return current;
};

export const checkTimeout = async (runningIsolated:(() => boolean), getTimeSecs: ((now:BigNumber) => Promise<BigNumber>), timeoutAt: TimeArg | undefined, nowTime: BigNumber): Promise<boolean> => {
  debug('checkTimeout', { timeoutAt, nowTime });
  if ( ! timeoutAt ) { return false; }
  const [ mode, val ] = timeoutAt;
  if ( mode === 'time' ) {
    return val.lte(nowTime);
  } else if ( mode === 'secs' ) {
    try {
      const nowSecs = await getTimeSecs(nowTime);
      return val.lte(nowSecs);
    } catch (e) {
      debug('checkTimeout','err', `${e}` );
      if ( runningIsolated() ) {
        const nowSecs = Math.floor(Date.now() / 1000);
        debug('checkTimeout','isolated',val.toString(),nowSecs);
        return val.lt(nowSecs - 1);
      }
      return false;
    }
  } else {
    throw new Error(`invalid TimeArg mode`);
  }
};

type Pred<X> = (x:X) => boolean;
type AsyncPred<X> = (x:X) => Promise<boolean>;
const neverTrue = async <X>(r:X) => (void(r), false);
type EQPeqResult<ProcTxn> =
  | { timeout: true, time: Time }
  | { timeout: false, txn: ProcTxn };
export interface IEventQueue<EQInitArgs, RawTxn, ProcTxn> {
  isInited : () => boolean,
  init: (args:EQInitArgs) => void,
  pushIgnore: (pred: Pred<RawTxn>) => void,
  peq: (lab: string, didTimeout: AsyncPred<Time>) => Promise<EQPeqResult<ProcTxn>>,
  deq: (dhead: string) => Promise<ProcTxn>,
};
export interface EQGetTxnsR<RawTxn> {
  txns: Array<RawTxn>,
  gtime: BigNumber|undefined,
};
export interface EQCtorArgs<EQInitArgs, RawTxn, ProcTxn> {
  raw2proc: (t:RawTxn) => ProcTxn,
  alwaysIgnored: Pred<RawTxn>,
  getTxns: (dhead:string, initArgs:EQInitArgs, ctime: Time, howMany: number) => Promise<EQGetTxnsR<RawTxn>>,
  getTxnTime: (x:RawTxn) => Time,
};
export const makeEventQueue = <EQInitArgs, RawTxn, ProcTxn>(ctorArgs:EQCtorArgs<EQInitArgs,RawTxn,ProcTxn>): IEventQueue<EQInitArgs, RawTxn, ProcTxn> => {
  const { raw2proc, alwaysIgnored, getTxns, getTxnTime } = ctorArgs;
  let initArgs: EQInitArgs|undefined = undefined;
  let ptxns: Array<ProcTxn> = [];
  let ctime: Time = bigNumberify(0);
  const customIgnore: Array<Pred<RawTxn>> = [];
  const isInited = () => initArgs !== undefined;
  const init = (args:EQInitArgs) => {
    assert(initArgs === undefined, `init: must be uninitialized`);
    initArgs = args;
  };
  const pushIgnore = (pred: Pred<RawTxn>) => {
    assert(initArgs !== undefined, `pushIgnore: must be initialized`);
    customIgnore.push(pred);
  };
  const notIgnored = (txn:RawTxn) => (! alwaysIgnored(txn));
  const peq = async (lab: string, didTimeout: AsyncPred<Time>): Promise<EQPeqResult<ProcTxn>> => {
    const dhead = `${lab} peq`;
    const updateCtime = (ntime:Time): Time => {
      if ( ctime.lt(ntime) ) {
        debug(dhead, 'updating ctime', { ctime, ntime });
        ctime = ntime;
      }
      return ntime;
    };
    if (initArgs === undefined) {
      throw Error(`${dhead}: not initialized`); }
    let howMany = 0;
    while ( ptxns.length === 0 ) {
      let { txns, gtime } = await getTxns(dhead, initArgs, ctime, howMany++);
      if ( txns.length === 0 && gtime ) { updateCtime(gtime); }
      else {
        const r = (x:RawTxn): Time => updateCtime(getTxnTime(x));
        const cmpTxn = (x:RawTxn, y:RawTxn): number =>
          r(x).sub(r(y)).toNumber();
        txns.sort(cmpTxn);
        if ( txns.length === 1 ) { r(txns[0]); }
        txns = txns.filter(notIgnored);
      }
      const cis = customIgnore;
      while ( txns.length > 0 && cis.length > 0 ) {
        const ci = cis[0];
        cis.shift();
        const t = txns[0];
        txns.shift();
        if ( ! ci(t) ) {
          throw Error(`${dhead} customIgnore present, ${ci}, but top txn did not match ${j2s(t)}`);
        } else {
          debug(dhead, `ignored`, ci, t);
        }
      }
      if ( txns.length === 0 && await didTimeout(ctime) ) {
        return { timeout: true, time: ctime };
      }
      ptxns = txns.map(raw2proc);
    }
    return { timeout: false, txn: ptxns[0] };
  };
  const deq = async (dhead: string): Promise<ProcTxn> => {
    const r = await peq(dhead, neverTrue);
    if ( r.timeout ) { throw Error('impossible'); }
    ptxns.shift();
    return r.txn;
  };

  return { isInited, init, peq, deq, pushIgnore };
};

export interface IMESArgs<EQInitArgs, RawTxn, ProcTxn, Log> {
  eq: IEventQueue<EQInitArgs, RawTxn, ProcTxn>
  getTxnTime: (x:ProcTxn) => Time,
  sync: () => Promise<void>,
  getNetworkTime: () => Promise<Time>,
  getLogs: (t:ProcTxn) => Array<Log>,
  parseLog: (l:Log) => (any[]|undefined),
};
export const makeEventStream = <EQInitArgs, RawTxn, ProcTxn, Log>(args:IMESArgs<EQInitArgs, RawTxn, ProcTxn, Log>) => {
  const { eq, getTxnTime, sync, getNetworkTime, getLogs, parseLog } = args;

  let time = bigNumberify(0);
  let logs: Log[] = [];
  const seek = (t: Time) => {
    assert(time.lt(t), 'seek must seek future');
    debug("EventStream::seek", t);
    time = t;
    logs = [];
  };
  const next = async () => {
    await sync();
    let dhead = "EventStream::next";
    let parsedLog = undefined;
    while ( parsedLog === undefined ) {
      while ( logs.length === 0 ) {
        const txn = await eq.deq(dhead);
        debug(dhead, { txn });
        const cr = getTxnTime(txn);
        if ( cr.gte(time) ) {
          time = cr;
          logs = getLogs(txn)
          debug(dhead, { time, logs });
        }
      }
      const l = logs[0];
      logs.shift();
      parsedLog = parseLog(l);
      debug(dhead, { parsedLog, l });
    }
    debug(dhead, 'ret');
    return { when: time, what: parsedLog };
  };
  const seekNow = async () => seek(await getNetworkTime());
  const lastTime = async () => time;
  const monitor = async (onEvent: (x: any) => void) => {
    while (true) { onEvent(await next()); }
  };
  return { lastTime, seek, seekNow, monitor, next };
};

export function getQueryLowerBound(): BigNumber {
  console.log(`WARNING: getQueryLowerBound() is deprecated and does nothing.`);
  return bigNumberify(0);
};
export function setQueryLowerBound(x: BigNumber|number): void {
  void(x);
  console.log(`WARNING: setQueryLowerBound() is deprecated and does nothing.`);
};

const makePromise = <A>(): [Promise<A>, (a:A) => any] => {
  let r = (a:A): any => { void(a); throw new Error(`promise never initialized`); };
  const p: Promise<A> = new Promise((resolve) => { r = resolve; });
  return [ p, r ];
};

export class Signal {
  p: Promise<boolean>;
  r: (a:boolean) => void;

  constructor() {
    [ this.p, this.r ] = makePromise();
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

export class Lock {
  locked: boolean;

  constructor() {
    this.locked = false;
  }
  async acquire(): Promise<void> {
    let x = 1;
    while ( this.locked ) {
      await Timeout.set(Math.min(512, x));
      x = x * 2;
    }
    this.locked = true;
  }
  release() {
    this.locked = false;
  }
  async runWith<X>(f: (() => Promise<X>)): Promise<X> {
    await this.acquire();
    try {
      const r = await f();
      this.release();
      return r;
    } catch (e:any) {
      this.release();
      throw e;
    }
  }
}

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

export const retryLoop = async <T>(lab: any, f: (() => Promise<T>)) => {
  let retries = 0;
  while ( true ) {
    try { return await f(); }
    catch (e:any) {
      console.log(`retryLoop`, { lab, retries, e });
      retries++;
    }
  }
};


type SigningMonitor = (e:any, pre:Promise<any>, post:Promise<any>) => void;
export type SetSigningMonitor = (h: SigningMonitor) => void;
export type NotifyComplete<A> = (post:Promise<A>) => Promise<A>;
export type NotifySend<A, B> = (e: any, pre:Promise<A>) => Promise<[A, NotifyComplete<B>]>;

export const makeSigningMonitor = <A,B>(): [SetSigningMonitor, NotifySend<A,B>] => {
  let mon: SigningMonitor|undefined = undefined;
  const setSigningMonitor: SetSigningMonitor = (h) => {
    mon = h;
  };
  const notifySend: NotifySend<A,B> = async (e:any, pre:Promise<A>) => {
    if ( mon ) {
      const [ post, postr ] = makePromise<B>();
      mon(e, pre, post);
      const notifyComplete: NotifyComplete<B> = async (pb:Promise<B>) => {
        const b = await pb;
        postr(b);
        return b;
      };
      return [ await pre, notifyComplete ];
    } else {
      return [ await pre, (x:Promise<B>) => x ];
    }
  };
  return [ setSigningMonitor, notifySend ];
};

/** @example lpad('asdf', '0', 6); // => '00asdf' */
const lpad = (str: string, padChar: string, nChars: number) => {
  const padding = padChar.repeat(Math.max(nChars - str.length, 0));
  return padding + str;
}

/** @example rdrop('asfdfff', 'f'); // => 'asfd' */
const rdrop = (str: string, char: string) => {
  while (str[str.length - 1] === char) {
    str = str.slice(0, str.length - 1);
  }
  return str;
}

/** @example ldrop('007', '0'); // => '7' */
const ldrop = (str: string, char: string) => {
  while (str[0] === char) {
    str = str.slice(1);
  }
  return str;
}

// Helper to<BigNumber> -> string formatting function used in a couple of places
// amt = the number to format
// decimals = number of digits from the right to put the decimal point
// splitValue = number of digits to keep after the decimal point
// Example: handleFormat(1234567, 4, 2) => "123.45"
export const handleFormat = (amt: unknown, decimals: number, splitValue: number = 6): string => {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  if (!(Number.isInteger(splitValue) && 0 <= splitValue)) {
    throw Error(`Expected split value to be a nonnegative integer, but got ${decimals}.`);
  }
  const amtStr = bigNumberify(amt).toString();
  const splitAt = Math.max(amtStr.length - splitValue, 0);
  const lPredropped = amtStr.slice(0, splitAt);
  const l = ldrop(lPredropped, '0') || '0';
  if (decimals === 0) { return l; }

  const rPre = lpad(amtStr.slice(splitAt), '0', splitValue);
  const rSliced = rPre.slice(0, decimals);
  const r = rdrop(rSliced, '0');

  return r ? `${l}.${r}` : l;
}

export const formatWithDecimals = (amt: unknown, decimals: number): string => {
  return handleFormat(amt, decimals, decimals)
}

export const apiStateMismatchError = (bin: IBackend<any>, es: BigNumber, as: BigNumber) : Error => {
  const formatLoc = (s:BigNumber) =>
    formatAssertInfo(bin._stateSourceMap[s.toNumber()]);
  const el = formatLoc(es);
  const al = formatLoc(as);
  return Error(`Expected the DApp to be in state ${es}, but it was actually in state ${as}.\n`
    + `\nState ${es} corresponds to the commit() at ${el}`
    + `\nState ${as} corresponds to the commit() at ${al}`
    + (el == al ? "\n(This means that the commit() is in the continuation of impure control-flow.)" : ""));
};
