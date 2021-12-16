export const connector = 'ALGO';

import algosdk from 'algosdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import buffer from 'buffer';
import type { Transaction } from 'algosdk'; // =>
import type {
  ARC11_Wallet,
  WalletTransaction,
  EnableNetworkResult,
  EnableAccountsResult,
} from './ALGO_ARC11'; // =>

const {Buffer} = buffer;

import {
  VERSION
} from './version';
import {
  CurrencyAmount, OnProgress,
  IViewLib, IBackend, IBackendViewInfo, IBackendViewsInfo,
  IRecvArgs, ISendRecvArgs,
  IAccount, IContract, IRecv,
  ISetupArgs, ISetupViewArgs, ISetupRes,
  // ISimRes,
  TimeArg,
  ISimTxn,
  stdContract, stdVerifyContract,
  stdAccount,
  debug, envDefault,
  argsSplit,
  makeRandom,
  replaceableThunk,
  ensureConnectorAvailable,
  bigNumberToBigInt,
  argMax,
  argMin,
  make_newTestAccounts,
  make_waitUntilX,
  checkTimeout,
  truthyEnv,
  Signal,
  Lock,
  retryLoop,
  Time,
  ISetupEventArgs,
} from './shared_impl';
import {
  isBigNumber,
  bigNumberify,
  bigNumberToNumber,
} from './shared_user';
import {
  CBR_Address, CBR_Val,
} from './CBR';
import waitPort from './waitPort';
import {
  Token,
  ALGO_Ty,
  NV,
  addressFromHex,
  stdlib,
  typeDefs,
  extractAddr,
} from './ALGO_compiled';
import { window, process, Env } from './shim';
import { sha512_256 } from 'js-sha512';
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = stdlib;
export * from './shared_user';

// Type Definitions

type BigNumber = ethers.BigNumber;

type AnyALGO_Ty = ALGO_Ty<CBR_Val>;
type ConnectorTy= AnyALGO_Ty;
// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX Copy/pasted type defs from types/algosdk
// This is so that this module can be exported without our custom types/algosdk
// The unused ones are commented out
type Address = string
// type RawAddress = Uint8Array;
type SecretKey = Uint8Array; // length 64

type TxnParams = {
  flatFee?: boolean,
  fee: number,
  firstRound: number,
  lastRound: number,
  genesisID: string,
  genesisHash: string,
}
type RecvTxn = {
  'confirmed-round': number,
  'application-index'?: number,
  'application-args': Array<string>,
  'sender': Address,
  'logs': Array<string>,
};
type TxId = string;
type ApiCall<T> = {
  do: () => Promise<T>,
};
type CompileResultBytes = {
  src: String,
  result: Uint8Array,
  hash: Address
};

type NetworkAccount = {
  addr: Address,
  sk?: SecretKey
};

const reachBackendVersion = 6;
const reachAlgoBackendVersion = 6;
type Backend = IBackend<AnyALGO_Ty> & {_Connectors: {ALGO: {
  version: number,
  appApproval: string,
  appClear: string,
  stateSize: number,
  stateKeys: number,
  mapDataSize: number,
  mapDataKeys: number,
  unsupported: Array<string>,
}}};
type BackendViewsInfo = IBackendViewsInfo<AnyALGO_Ty>;
type BackendViewInfo = IBackendViewInfo<AnyALGO_Ty>;

type CompiledBackend = {
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
};

type ContractInfo = number;
type SendRecvArgs = ISendRecvArgs<Address, Token, AnyALGO_Ty>;
type RecvArgs = IRecvArgs<AnyALGO_Ty>;
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Address, Token, AnyALGO_Ty>;
type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
type SimTxn = ISimTxn<Token>
type SetupArgs = ISetupArgs<ContractInfo, VerifyResult>;
type SetupViewArgs = ISetupViewArgs<ContractInfo, VerifyResult>;
type SetupEventArgs = ISetupEventArgs<ContractInfo, VerifyResult>;
type SetupRes = ISetupRes<ContractInfo, Address, Token, AnyALGO_Ty>;

// Helpers

// Parse CBR into Public Key
const cbr2algo_addr = (x:string): Address =>
  algosdk.encodeAddress(Buffer.from(x.slice(2), 'hex'));

const txnFromAddress = (t:Transaction): Address =>
  algosdk.encodeAddress(t.from.publicKey);

function uint8ArrayToStr(a: Uint8Array, enc: 'utf8' | 'base64' = 'utf8') {
  if (!(a instanceof Uint8Array)) {
    console.log(a);
    throw Error(`Expected Uint8Array, got ${a}`);
  }
  return Buffer.from(a).toString(enc);
}

// TODO: read token from scripts/devnet-algo/algorand_data/algod.token
const rawDefaultToken = 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const rawDefaultItoken = 'reach-devnet';

type OrExn<X> = X | {exn:any};
type IndexerTxn = any;
type AlgodTxn = {
  'application-index'?: number,
  'confirmed-round'?: number,
  'logs'?: Array<string>,
  'txn': {
    'sig': Uint8Array,
    'txn': any,
  },
  'pool-error': string,
};

const indexerTxn2RecvTxn = (txn:any) => {
  const ait = txn['application-transaction'] || {};
  const aargs = ait['application-args'] || [];
  const aidx = ait['application-id'];
  return {
    'confirmed-round': txn['confirmed-round'],
    'sender': txn['sender'],
    'logs': (txn['logs'] || []),
    'application-args': aargs,
    'application-index': aidx,
  };
};

const waitForConfirmation = async (txId: TxId, untilRound?: number|undefined): Promise<RecvTxn> => {
  const doOrDie = async <X>(p: Promise<X>): Promise<OrExn<X>> => {
    try { return await p; }
    catch (e:any) { return { 'exn': e }; }
  };
  const checkTooLate = async (lastLastRound: number): Promise<number> => {
    const [ c, msg ] = lastLastRound > 0 ?
      [ client.statusAfterBlock(lastLastRound),
        `waiting until after ${lastLastRound}` ] :
      [ client.status(),
        `looking up current round` ];
    debug(...dhead, msg);
    const lastRound = (await c.do())['last-round'];
    if ( untilRound && untilRound < lastRound ) {
      throw Error(`waitForConfirmation: Too late: ${lastRound} > ${untilRound}`);
    } else {
      return lastRound;
    }
  };

  const dhead = [ 'waitForConfirmation', txId ];
  const client = await getAlgodClient();

  const checkAlgod = async (lastLastRound:number): Promise<RecvTxn> => {
    const lastRound = await checkTooLate(lastLastRound);
    const info =
      (await doOrDie(client.pendingTransactionInformation(txId).do())) as OrExn<AlgodTxn>;
    debug(...dhead, 'info', info);
    if ( 'exn' in info ) {
      debug(...dhead, 'switching to indexer on error');
      return await checkIndexer(lastRound);
    }
    const cr = info['confirmed-round'];
    if ( cr !== undefined && cr > 0 ) {
      const l = info['logs'] === undefined ? [] : info['logs'];
      debug(...dhead, 'confirmed');
      const dtxn = algosdk.Transaction.from_obj_for_encoding(info['txn']['txn']);
      debug(...dhead, 'confirmed', dtxn);
      return {
        'confirmed-round': cr,
        'logs': l,
        'application-index': info['application-index'],
        'sender': txnFromAddress(dtxn),
        'application-args': (dtxn.appArgs || []).map((x)=> uint8ArrayToStr(x, 'base64')),
      };
    } else if ( info['pool-error'] === '' ) {
      debug(...dhead, 'still in pool, trying again');
      return await checkAlgod(lastRound);
    } else {
      throw Error(`waitForConfirmation: error confirming: ${JSON.stringify(info)}`);
    }
  };

  const checkIndexer = async (lastLastRound: number): Promise<RecvTxn> => {
    const lastRound = await checkTooLate(lastLastRound);
    const indexer = await getIndexer();
    const q = indexer.lookupTransactionByID(txId);
    const res = (await doOrDie(doQuery_(JSON.stringify(dhead), q))) as OrExn<IndexerTxn>;
    debug(...dhead, 'indexer', res);
    if ( 'exn' in res ) {
      debug(...dhead, 'indexer failed, trying again');
      return await checkIndexer(lastRound);
    } else {
      return indexerTxn2RecvTxn(res['transaction']);
    }
  };

  return await checkAlgod(0);
};

const decodeB64Txn = (ts:string): Transaction => {
  const tb = Buffer.from(ts, 'base64');
  return algosdk.decodeUnsignedTransaction(tb);
};

const doSignTxnToB64 = (t:Transaction, sk:SecretKey): string => {
  const sb = Buffer.from(t.signTxn(sk));
  return sb.toString('base64');
};

const doSignTxn = (ts:string, sk:SecretKey): string => {
  return doSignTxnToB64(decodeB64Txn(ts), sk);
};

const signSendAndConfirm = async (
  acc: NetworkAccount,
  txns: Array<WalletTransaction>,
): Promise<RecvTxn> => {
  if ( acc.sk !== undefined ) {
    txns.forEach((t:WalletTransaction): void => {
      // XXX this comparison is probably wrong, because the addresses are the
      // wrong type
      if ( acc.sk !== undefined && ! t.stxn && t.signers !== undefined && t.signers.length === 1 && t.signers[0] === acc.addr ) {
        debug('signSendAndConfirm', 'signing one');
        t.stxn = doSignTxn(t.txn, acc.sk);
      }
    });
  }
  const p = await getProvider();
  try {
    await p.signAndPostTxns(txns);
  } catch (e) {
    throw { type: 'signAndPost', e };
  }
  const N = txns.length - 1;
  const tN = decodeB64Txn(txns[N].txn);
  try {
    return await waitForConfirmation(tN.txID(), tN.lastRound);
  } catch (e) {
    throw { type: 'waitForConfirmation', e };
  }
};

const encodeUnsignedTransaction = (t:Transaction): string => {
  return Buffer.from(algosdk.encodeUnsignedTransaction(t)).toString('base64');
};

const toWTxn = (t:Transaction): WalletTransaction => {
  return {
    txn: encodeUnsignedTransaction(t),
    signers: [ txnFromAddress(t) ],
  };
};

// Backend
const compileTEAL = async (label: string, code: string): Promise<CompileResultBytes> => {
  debug('compile', label);
  let s, r;
  try {
    r = await (await getAlgodClient()).compile(code).do();
    s = 200;
  } catch (e:any) {
    s = (e && typeof e === 'object') ? e.statusCode : 'not object';
    r = e;
  }

  if ( s == 200 ) {
    debug('compile',  label, 'succeeded:', r);
    r.src = code;
    r.result = base64ToUI8A(r.result);
    // debug('compile transformed:', r);
    return r;
  } else {
    throw Error(`compile ${label} failed: ${s}: ${JSON.stringify(r)}`);
  }
};

const getTxnParams = async (): Promise<TxnParams> => {
  debug(`fillTxn: getting params`);
  const client = await getAlgodClient();
  while (true) {
    const params = await client.getTransactionParams().do();
    debug('fillTxn: got params:', params);
    if (params.firstRound !== 0) {
      return params;
    }
    debug(`...but firstRound is 0, so let's wait and try again.`);
    await client.statusAfterBlock(1).do();
  }
};

const sign_and_send_sync = async (
  label: string,
  acc: NetworkAccount,
  txn: WalletTransaction,
): Promise<RecvTxn> => {
  try {
    return await signSendAndConfirm(acc, [txn]);
  } catch (e) {
    console.log(e);
    throw Error(`${label} txn failed:\n${JSON.stringify(txn)}\nwith:\n${JSON.stringify(e)}`);
  }
};

// XXX I'd use x.replaceAll if I could (not supported in this node version), but it would be better to extend ConnectorInfo so these are functions
const replaceAll = (orig: string, what: string, whatp: string): string => {
  const once = orig.replace(what, whatp);
  if ( once === orig ) {
    return orig;
  } else {
    return replaceAll(once, what, whatp);
  }
};

function must_be_supported(bin: Backend) {
  const algob = bin._Connectors.ALGO;
  const { unsupported } = algob;
  if ( unsupported.length > 0 ) {
    const reasons = unsupported.map(s => ` * ${s}`).join('\n');
    throw Error(`This Reach application is not supported on Algorand for the following reasons:\n${reasons}`);
  }
}

// Get these from stdlib
// const MaxTxnLife = 1000;
const MinTxnFee = 1000;
const MaxAppProgramLen = 2048;
const MaxAppTxnAccounts = 4;
const MaxExtraAppProgramPages = 3;
const MinBalance = 100000;

async function compileFor(bin: Backend): Promise<CompiledBackend> {
  must_be_supported(bin);
  const { appApproval, appClear } = bin._Connectors.ALGO;

  const checkLen = (label:string, actual:number, expected:number): void => {
    debug(`checkLen`, {label, actual, expected});
    if ( actual > expected ) {
        throw Error(`This Reach application is not supported by Algorand: ${label} length is ${actual}, but should be less than ${expected}.`); } };

  const appApproval_bin =
    await compileTEAL('appApproval_subst', appApproval);
  const appClear_bin =
    await compileTEAL('appClear', appClear);
  checkLen(`App Program Length`, (appClear_bin.result.length + appApproval_bin.result.length), (1 + MaxExtraAppProgramPages) * MaxAppProgramLen);

  return {
    appApproval: appApproval_bin,
    appClear: appClear_bin,
  };
}

const ui8h = (x:Uint8Array): string => Buffer.from(x).toString('hex');
const base64ToUI8A = (x:string): Uint8Array => Uint8Array.from(Buffer.from(x, 'base64'));
const base64ify = (x: any): String => Buffer.from(x).toString('base64');

const format_failed_request = (e: any) => {
  const ep = JSON.parse(JSON.stringify(e));
  const db64 =
    ep.req ?
    (ep.req.data ? base64ify(ep.req.data) :
     `no data, but ${JSON.stringify(Object.keys(ep.req))}`) :
     `no req, but ${JSON.stringify(Object.keys(ep))}`;
  const msg = e.text ? JSON.parse(e.text) : e;
  return `\n${db64}\n${JSON.stringify(msg)}`;
};

function looksLikeAccountingNotInitialized(e: any) {
  const responseText = e?.response?.text || null;
  // TODO: trust the response to be json and parse it?
  // const json = JSON.parse(responseText) || {};
  // const msg: string = (json.message || '').toLowerCase();
  const msg = (responseText || '').toLowerCase();
  return msg.includes(`accounting not initialized`);
}

const doQuery_ = async <T>(dhead:string, query: ApiCall<T>, alwaysRetry: boolean = false): Promise<T> => {
  debug(dhead, '--- QUERY =', query);
  let retries = 10;
  let res;
  while ( retries > 0 ) {
    try {
      res = await query.do();
      break;
    } catch (e:any) {
      if ( e?.errno === -111 || e?.code === "ECONNRESET") {
        debug(dhead, 'NO CONNECTION');
      } else if ( looksLikeAccountingNotInitialized(e) ) {
        debug(dhead, 'ACCOUNTING NOT INITIALIZED');
      } else if ( ! alwaysRetry || retries <= 0 ) {
        throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`); // `
      }
      debug(dhead, 'RETRYING', retries--, {e});
      await Timeout.set(500);
    }
  }
  if (!res) { throw Error(`impossible: query res is empty`); }
  debug(dhead, 'RESULT', res);
  return res;
};

type QueryResult =
  | { succ: true, txn: any }
  | { succ: false, round: number }

// ****************************************************************************
// Event Cache
// ****************************************************************************

const chooseMinRoundTxn = (ptxns: any[]) =>
  argMin(ptxns, (x: any) => x['confirmed-round']);

const chooseMaxRoundTxn = (ptxns: any[]) =>
  argMax(ptxns, (x: any) => x['confirmed-round']);

type QueryInfo = {
  minRound?: number,
  timeoutAt?: TimeArg,
  specRound?: number,
  isEventStream?: boolean,
}

const [_getQueryLowerBound, _setQueryLowerBound] = replaceableThunk<number>(() => 0);
const [getValidQueryWindow, _setValidQueryWindow] = replaceableThunk<number|true>(() => true);

export {getValidQueryWindow};
export function setValidQueryWindow(n: number|true): void {
  if (typeof n === 'number') {
    // TODO?
    throw Error(`Only setValidQueryWindow(true) is supported on Algorand`);
  }
  _setValidQueryWindow(n);
}

export function getQueryLowerBound(): BigNumber {
  return bigNumberify(_getQueryLowerBound());
}

export function setQueryLowerBound(networkTime: BigNumber|number): void {
  networkTime = typeof networkTime === 'number' ? networkTime
    : networkTime._isBigNumber ? networkTime.toNumber()
    : networkTime;
  if (!(typeof networkTime === 'number')) { throw Error(`Expected number or BigNumber, but got ${networkTime} : ${typeof networkTime}`);}
  _setQueryLowerBound(networkTime);
}

class EventCache {

  cache: any[] = [];

  currentRound: number;

  constructor() {
    this.currentRound = _getQueryLowerBound();
    this.cache = [];
  }

  async query(dhead: string, ApplicationID: number, queryInfo: QueryInfo, pred: ((x:any) => boolean), choose : (x: any[]) => any = chooseMinRoundTxn): Promise<QueryResult> {
    const { minRound, timeoutAt, specRound, isEventStream = false } = queryInfo;
    const h = (mode:string): (number | undefined) => timeoutAt && timeoutAt[0] === mode ? bigNumberToNumber(timeoutAt[1]) : undefined;
    const maxRound = h('time');
    const maxSecs = h('secs');
    debug(dhead, `EventCache.query`, {ApplicationID, minRound, specRound, timeoutAt, maxRound, maxSecs}, this.currentRound);

    // Clear cache of stale transactions.
    // Cache's min bound will be `minRound || specRound`
    const filterRound = minRound ?? specRound!;
    this.cache = this.cache.filter((txn) => {
      const notTooOld = txn['confirmed-round'] >= filterRound;
      const emptyOptIn =
        (  (txn['application-transaction']['on-completion'] === 'optin')
        && (txn['application-transaction']['application-args'].length == 0));
      return notTooOld && (! emptyOptIn);
    });

    // When checking predicate, only choose transactions that are below
    // max round, or the specific round we're looking for.
    const filterFn = (x: any) => pred(x)
      && (maxRound ? x['confirmed-round'] <= maxRound : true)
      && (maxSecs ? x['round-time'] <= maxSecs : true)
      && (specRound ? x['confirmed-round'] == specRound : true);

    // Check to see if the transaction we want is in cache
    const initPtxns = this.cache.filter(filterFn);

    if (initPtxns.length != 0) {
      debug(`Found transaction in Event Cache`);
      const txn = choose(initPtxns)
      return { succ: true, txn };
    }
    debug(`transaction not in event cache`);

    const failed = (): {succ: false, round: number} => ({ succ: false, round: this.currentRound });
    if ( this.cache.length != 0 && !isEventStream ) {
      debug(`cache not empty, contains some other message from future, not querying...`, this.cache);
      return failed();
    }

    debug(`querying network...`);
    // If no results, then contact network
    const indexer = await getIndexer();

    let query =
      indexer.searchForTransactions()
        .applicationID(ApplicationID)
        .txType('appl')

    if (filterRound) {
      // If cache has: [100, 200]
      // & querying  : [150, 1000]
      // We already searched cache for [150, 200] so query network for [201, 1000]
      query = query.minRound(Math.max(this.currentRound + 1, filterRound));
    }

    const res: any = await doQuery_(dhead, query);
    this.cache = res.transactions;

    // Update current round
    this.currentRound =
      (res.transactions.length == 0)
        ? (maxRound ? Math.min(res['current-round'], maxRound) : res['current-round'])
        : chooseMaxRoundTxn(res.transactions)['confirmed-round'];

    // Check for pred again
    const ptxns = this.cache.filter(filterFn);

    if ( ptxns.length == 0 ) {
      return failed();
    }

    const txn = choose(ptxns);

    return { succ: true, txn };
  }
}

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

export const { addressEq, tokenEq, digest } = stdlib;

export const { T_Null, T_Bool, T_UInt, T_Tuple, T_Array, T_Contract, T_Object, T_Data, T_Bytes, T_Address, T_Digest, T_Struct, T_Token } = typeDefs;

export const { randomUInt, hasRandom } = makeRandom(8);

async function waitIndexerFromEnv(env: ProviderEnv): Promise<algosdk.Indexer> {
  const { ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT, ALGO_INDEXER_TOKEN } = env;
  await waitPort(ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);
  return new algosdk.Indexer(ALGO_INDEXER_TOKEN, ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);
}

async function waitAlgodClientFromEnv(env: ProviderEnv): Promise<algosdk.Algodv2> {
  const { ALGO_SERVER, ALGO_PORT, ALGO_TOKEN } = env;
  await waitPort(ALGO_SERVER, ALGO_PORT);
  return new algosdk.Algodv2(ALGO_TOKEN, ALGO_SERVER, ALGO_PORT);
}

// This function should be provided by the indexer, but it isn't so we simulate
// something decent. This function is allowed to "fail" by not really waiting
// until the round
const indexer_statusAfterBlock = async (round: number): Promise<BigNumber> => {
  debug('indexer_statusAfterBlock', {round});
  const client = await getAlgodClient();
  let now = bigNumberify(0);
  // When we're isolated, sometimes we wait for blocks that will never happen.
  // This is a protection against that.
  let tries = 0;
  while ( (tries++ < 10) && (now = await getNetworkTime()).lt(round) ) {
    debug('indexer_statusAfterBlock', {round, now});
    await client.statusAfterBlock(round);
    // XXX Get the indexer to index one and wait
    await Timeout.set(500);
  }
  return now;
};

interface Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: any) => Promise<any>,
};

const makeProviderByWallet = async (wallet:ARC11_Wallet): Promise<Provider> => {
  debug(`making provider with wallet`);
  const walletOpts = {'network': process.env['ALGO_NETWORK']};
  let enabledNetwork: EnableNetworkResult|undefined;
  let enabledAccounts: EnableAccountsResult|undefined;
  if ( wallet.enableNetwork === undefined && wallet.enableAccounts === undefined ) {
    const enabled = await wallet.enable(walletOpts);
    enabledNetwork = enabled;
    enabledAccounts = enabled;
  } else if ( wallet.enableNetwork === undefined || wallet.enableAccounts === undefined ) {
    throw new Error('must have enableNetwork AND enableAccounts OR neither');
  } else {
    enabledNetwork = await wallet.enableNetwork(walletOpts);
  }
  void enabledNetwork;
  const algodClient = await wallet.getAlgodv2();
  const indexer = await wallet.getIndexer();
  const getDefaultAddress = async (): Promise<Address> => {
    if ( enabledAccounts === undefined ) {
      if ( wallet.enableAccounts === undefined ) {
        throw new Error('impossible: no wallet.enableAccounts');
      }
      enabledAccounts = await wallet.enableAccounts(walletOpts);
      if ( enabledAccounts === undefined ) {
        throw new Error('Could not enable accounts');
      }
    }
    return enabledAccounts.accounts[0];
  };
  const signAndPostTxns = wallet.signAndPostTxns;
  const isIsolatedNetwork = truthyEnv(process.env['REACH_ISOLATED_NETWORK']);
  return { algodClient, indexer, getDefaultAddress, isIsolatedNetwork, signAndPostTxns };
};

export const setWalletFallback = (wf:() => any) => {
  if ( ! window.algorand ) { window.algorand = wf(); }
};
const doWalletFallback_signOnly = (opts:any, getAddr:() => Promise<string>, signTxns:(txns:string[]) => Promise<string[]>): ARC11_Wallet => {
  let p: Provider|undefined = undefined;
  const enableNetwork = async (eopts?:any) => {
    void(eopts);
    const base = opts['providerEnv'];
    let baseEnv: Env = process.env;
    if ( base ) {
      if ( typeof base === 'string' ) {
        // @ts-ignore
        baseEnv = await providerEnvByName(base);
      } else {
        baseEnv = base;
      }
    }
    p = await makeProviderByEnv(baseEnv);
    return {};
  };
  const enableAccounts = async (eopts?:any) => {
    void(eopts);
    const addr = await getAddr();
    return { accounts: [ addr ] };
  };
  const enable = async (eopts?:any) => {
    await enableNetwork(eopts);
    return await enableAccounts(eopts);
  };
  const getAlgodv2 = async () => {
    if ( !p ) { throw new Error(`must call enable`) };
    return p.algodClient;
  };
  const getIndexer = async () => {
    if ( !p ) { throw new Error(`must call enable`) };
    return p.indexer;
  };
  const signAndPostTxns = async (txns:WalletTransaction[], sopts?:any) => {
    if ( !p ) { throw new Error(`must call enable`) };
    void(sopts);
    debug(`fallBack: signAndPostTxns`, {txns});
    const to_sign: string[] = [];
    txns.forEach((txn) => {
      if ( ! txn.stxn ) {
        to_sign.push(txn.txn);
      }
    });
    debug(`fallBack: signAndPostTxns`, {to_sign});
    const signed: string[] = to_sign.length == 0 ? [] : await signTxns(to_sign);
    debug(`fallBack: signAndPostTxns`, {signed});
    const stxns: string[] = txns.map((txn) => {
      if ( txn.stxn ) { return txn.stxn; }
      const s = signed.shift();
      if ( ! s ) { throw new Error(`txn not signed`); }
      return s;
    });
    const bs = stxns.map((stxn) => Buffer.from(stxn, 'base64'));
    debug(`fallBack: signAndPostTxns`, bs);
    await p.algodClient.sendRawTransaction(bs).do();
    return {};
  };
  return { enable, enableNetwork, enableAccounts, getAlgodv2, getIndexer, signAndPostTxns };
};
const walletFallback_mnemonic = (opts:any) => (): ARC11_Wallet => {
  debug(`using mnemonic wallet fallback`);
  const getAddr = async (): Promise<string> => {
    return window.prompt(`Please paste the address of your account:`);
  };
  const signTxns = async (txns: string[]): Promise<string[]> => {
    return txns.map((ts) => {
      const t = decodeB64Txn(ts);
      const addr = txnFromAddress(t);
      const mn = window.prompt(`Please paste the mnemonic for the address, ${addr}. It will not be saved.`);
      const acc = algosdk.mnemonicToSecretKey(mn);
      return doSignTxnToB64(t, acc.sk);
    });
  };
  return doWalletFallback_signOnly(opts, getAddr, signTxns);
};
const walletFallback_MyAlgoWallet = (MyAlgoConnect:any, opts:any) => (): ARC11_Wallet => {
  debug(`using MyAlgoWallet wallet fallback`);
  // @ts-ignore
  const mac = new MyAlgoConnect();
  // MyAlgoConnect uses a global popup object for managing, so we need to
  // guarantee there is only one in flight at a time.
  const lock = new Lock();
  const getAddr = async (): Promise<string> => {
    const accts =
      await lock.runWith(async () => {
        return await mac.connect({shouldSelectOneAccount: true});
      });
    return accts[0].address;
  };
  const signTxns = async (txns: string[]): Promise<string[]> => {
    debug(`MAW signTransaction ->`, txns);
    const stxns: Array<{blob: Uint8Array}> =
      await lock.runWith(async () => {
        return await mac.signTransaction(txns);
      });
    debug(`MAW signTransaction <-`, stxns);
    return stxns.map((sts) => Buffer.from(sts.blob).toString('base64'));
  };
  return doWalletFallback_signOnly(opts, getAddr, signTxns);
};
const walletFallback_WalletConnect = (WalletConnect:any, opts:any) => (): ARC11_Wallet => {
  debug(`using WalletConnect wallet fallback`);
  const wc = new WalletConnect();
  return doWalletFallback_signOnly(opts, (() => wc.getAddr()), ((ts) => wc.signTxns(ts)));
};
export const walletFallback = (opts:any) => {
  debug(`using wallet fallback with`, opts);
  const mac = opts.MyAlgoConnect;
  if ( mac ) {
    return walletFallback_MyAlgoWallet(mac, opts);
  }
  const wc = opts.WalletConnect;
  if ( wc ) {
    return walletFallback_WalletConnect(wc, opts);
  }
  // This could be implemented with walletFallback_signOnly and the residue
  // from the old version.
  //  return walletFallback_AlgoSigner(opts);
  return walletFallback_mnemonic(opts);
};

export const [getProvider, setProvider] = replaceableThunk(async () => {
  if ( window.algorand ) {
    // @ts-ignore
    return await makeProviderByWallet(window.algorand);
  } else {
    debug(`making default provider based on process.env`);
    return await makeProviderByEnv(process.env);
  }
});
const getAlgodClient = async () => (await getProvider()).algodClient;
const getIndexer = async () => (await getProvider()).indexer;

export interface ProviderEnv {
  ALGO_SERVER: string
  ALGO_PORT: string
  ALGO_TOKEN: string
  ALGO_INDEXER_SERVER: string
  ALGO_INDEXER_PORT: string
  ALGO_INDEXER_TOKEN: string
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}

const localhostProviderEnv: ProviderEnv = {
  ALGO_SERVER: 'http://localhost',
  ALGO_PORT: '4180',
  ALGO_TOKEN: rawDefaultToken,
  ALGO_INDEXER_SERVER: 'http://localhost',
  ALGO_INDEXER_PORT: '8980',
  ALGO_INDEXER_TOKEN: rawDefaultItoken,
  REACH_ISOLATED_NETWORK: 'yes',
}

function envDefaultsALGO(env: Partial<ProviderEnv>): ProviderEnv {
  const denv = localhostProviderEnv;
  // @ts-ignore
  const ret: ProviderEnv = {};
  for ( const f of ['ALGO_SERVER', 'ALGO_PORT', 'ALGO_TOKEN', 'ALGO_INDEXER_SERVER', 'ALGO_INDEXER_PORT', 'ALGO_INDEXER_TOKEN', 'REACH_ISOLATED_NETWORK'] ) {
    // @ts-ignore
    ret[f] = envDefault(env[f], denv[f]);
  }
  return ret;
};

async function makeProviderByEnv(env: Partial<ProviderEnv>): Promise<Provider> {
  debug(`makeProviderByEnv`, env);
  const fullEnv = envDefaultsALGO(env);
  debug(`makeProviderByEnv defaulted`, fullEnv);
  const algodClient = await waitAlgodClientFromEnv(fullEnv);
  const indexer = await waitIndexerFromEnv(fullEnv);
  const isIsolatedNetwork = truthyEnv(fullEnv.REACH_ISOLATED_NETWORK);
  const lab = `Providers created by environment`;
  const getDefaultAddress = async (): Promise<Address> => {
    throw new Error(`${lab} do not have default addresses`);
  };
  const signAndPostTxns = async (txns:WalletTransaction[], opts?:any) => {
    void(opts);
    const stxns = txns.map((txn) => {
      if ( txn.stxn ) { return txn.stxn; }
      throw new Error(`${lab} cannot interactively sign`);
    });
    const bs = stxns.map((stxn) => Buffer.from(stxn, 'base64'));
    debug(`signAndPostTxns`, bs);
    await algodClient.sendRawTransaction(bs).do();
  };
  return { algodClient, indexer, isIsolatedNetwork, getDefaultAddress, signAndPostTxns };
};
export function setProviderByEnv(env: Partial<ProviderEnv>): void {
  setProvider(makeProviderByEnv(env));
};

function randlabsProviderEnv(net: string): ProviderEnv {
  const prefix = net === 'MainNet' ? '' : `${net.toLowerCase()}.`;
  const RANDLABS_BASE = `${prefix}algoexplorerapi.io`;
  return {
    ALGO_SERVER: `https://${RANDLABS_BASE}`,
    ALGO_PORT: '',
    ALGO_TOKEN: '',
    ALGO_INDEXER_SERVER: `https://algoindexer.${RANDLABS_BASE}`,
    ALGO_INDEXER_PORT: '',
    ALGO_INDEXER_TOKEN: '',
    REACH_ISOLATED_NETWORK: 'no',
  }
}

export function providerEnvByName(providerName: string): ProviderEnv {
  switch (providerName) {
    case 'MainNet': return randlabsProviderEnv('MainNet');
    case 'TestNet': return randlabsProviderEnv('TestNet');
    case 'BetaNet': return randlabsProviderEnv('BetaNet');
    case 'randlabs/MainNet': return randlabsProviderEnv('MainNet');
    case 'randlabs/TestNet': return randlabsProviderEnv('TestNet');
    case 'randlabs/BetaNet': return randlabsProviderEnv('BetaNet');
    case 'LocalHost': return localhostProviderEnv;
    default: throw Error(`Unrecognized provider name: ${providerName}`);
  }
}

export function setProviderByName(providerName: string): void {
  return setProviderByEnv(providerEnvByName(providerName));
}

// eslint-disable-next-line max-len
const rawFaucetDefaultMnemonic = 'frown slush talent visual weather bounce evil teach tower view fossil trip sauce express moment sea garbage pave monkey exercise soap lawn army above dynamic';
export const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  const FAUCET = algosdk.mnemonicToSecretKey(
    envDefault(process.env.ALGO_FAUCET_PASSPHRASE, rawFaucetDefaultMnemonic),
  );
  return await connectAccount(FAUCET);
});

const str2note = (x:string) => new Uint8Array(Buffer.from(x));
const NOTE_Reach_str = `Reach ${VERSION}`;
const NOTE_Reach = str2note(NOTE_Reach_str);
const NOTE_Reach_tag = (tag:any) => tag ? str2note(NOTE_Reach_str + ` ${tag})`) : NOTE_Reach;

const makeTransferTxn = (
  from: Address,
  to: Address,
  value: BigNumber,
  token: Token|undefined,
  ps: TxnParams,
  closeTo: Address|undefined = undefined,
  tag: number|undefined = undefined,
): Transaction => {
  const valuen = bigNumberToBigInt(value);
  const note = NOTE_Reach_tag(tag);
  const txn =
    token ?
      algosdk.makeAssetTransferTxnWithSuggestedParams(
        from, to, closeTo, undefined,
        valuen, note, bigNumberToNumber(token), ps)
    :
      algosdk.makePaymentTxnWithSuggestedParams(
        from, to, valuen, closeTo, note, ps);
  return txn;
};

export const transfer = async (
  from: Account,
  to: Account,
  value: any,
  token: Token|undefined = undefined,
  tag: number|undefined = undefined,
): Promise<RecvTxn> => {
  const sender = from.networkAccount;
  const receiver = extractAddr(to);
  const valuebn = bigNumberify(value);
  const ps = await getTxnParams();
  const txn = toWTxn(makeTransferTxn(sender.addr, receiver, valuebn, token, ps, undefined, tag));

  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuebn}`,
    sender,
    txn);
};

// XXX need to make this a log
const makeIsMethod = (i:number) => (txn:any): boolean => {
  const act = txn['application-transaction']['application-args'][0];
  const exp = base64ify([i]);
  const r = act === exp;
  //debug(`makeIsMethod`, {txn,i,act,exp,r});
  return r;
}

/** @description base64->hex->arrayify */
const reNetify = (x: string): NV => {
  const s: string = Buffer.from(x, 'base64').toString('hex');
  return ethers.utils.arrayify('0x' + s);
};

export const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const thisAcc = networkAccount;
  let label = thisAcc.addr.substring(2, 6);
  const pks = T_Address.canonicalize(thisAcc);
  debug(label, 'connectAccount');

  const selfAddress = (): CBR_Address => {
    return pks;
  };

  const iam = (some_addr: string): string => {
    if (some_addr === pks) {
      return some_addr;
    } else {
      throw Error(`I should be ${some_addr}, but am ${pks}`);
    }
  };

  const contract = (
    bin: Backend,
    givenInfoP?: Promise<ContractInfo>,
  ): Contract => {
    ensureConnectorAvailable(bin, 'ALGO', reachBackendVersion, reachAlgoBackendVersion);
    must_be_supported(bin);

    const { stateSize, stateKeys, mapDataKeys, mapDataSize } = bin._Connectors.ALGO;
    const hasMaps = mapDataKeys > 0;
    const { mapDataTy } = bin._getMaps({reachStdlib: stdlib});
    const emptyMapDataTy = T_Bytes(mapDataTy.netSize);
    const emptyMapData =
      // This is a bunch of Nones
      mapDataTy.fromNet(
        emptyMapDataTy.toNet(emptyMapDataTy.canonicalize('')));
    debug({ emptyMapData });

    type GlobalState = [BigNumber, BigNumber, Address];
    type ContractHandler = {
      ApplicationID: number,
      Deployer: Address,
      getLastRound: (() => number),
      setLastRound: ((x:number) => void),
      getLocalState: ((a:Address) => Promise<any>),
      ensureOptIn: (() => Promise<void>),
      getAppState: (() => Promise<any>),
      getGlobalState: ((appSt_g?:any) => Promise<GlobalState|undefined>),
      canIWin: ((lct:BigNumber) => Promise<boolean>),
      isIsolatedNetwork: (() => boolean),
      ctcAddr: Address,
    };

    const makeGetC = (setupViewArgs: SetupViewArgs, eventCache: EventCache, informCreationBlock: (cb: number) => void) => {
      const { getInfo: fake_getInfo } = setupViewArgs;
      let _theC: ContractHandler|undefined = undefined;
      return async (): Promise<ContractHandler> => {
        if ( _theC ) { return _theC; }
        const ctcInfo = await fake_getInfo();
        const { ApplicationID, Deployer, startRound } =
          await stdVerifyContract( setupViewArgs, (async () => {
            return await verifyContract_(label, ctcInfo, bin, eventCache);
          }));
        debug(label, 'getC', {ApplicationID, startRound} );

        informCreationBlock(startRound);
        const ctcAddr = algosdk.getApplicationAddress(ApplicationID);
        debug(label, 'getC', { ctcAddr });

        let realLastRound = startRound;
        const getLastRound = () => realLastRound;
        const setLastRound = (x:number) => (realLastRound = x);

        // Read map data
        const getLocalState = async (a:Address): Promise<any> => {
          const client = await getAlgodClient();
          const ai = await client.accountInformation(a).do();
          debug(`getLocalState`, ai);
          const als = ai['apps-local-state'].find((x:any) => (x.id === ApplicationID));
          debug(`getLocalState`, als);
          return als ? als['key-value'] : undefined;
        };

        // Application Local State Opt-in
        const didOptIn = async (): Promise<boolean> =>
          ((await getLocalState(thisAcc.addr)) !== undefined);
        const doOptIn = async (): Promise<void> => {
          debug(`doOptIn`);
          await sign_and_send_sync(
            'ApplicationOptIn',
            thisAcc,
            toWTxn(algosdk.makeApplicationOptInTxn(
              thisAcc.addr, await getTxnParams(),
              ApplicationID,
              undefined, undefined, undefined, undefined,
              NOTE_Reach)));
          assert(await didOptIn(), `didOptIn after doOptIn`);
        };
        let ensuredOptIn: boolean = false;
        const ensureOptIn = async (): Promise<void> => {
          if ( ! ensuredOptIn ) {
            if ( ! await didOptIn() ) {
              await doOptIn();
            }
            ensuredOptIn = true;
          }
        };

        const getAppState = async (): Promise<any> => {
          const lab = `getAppState`;
          const client = await getAlgodClient();
          let appInfo;
          try {
            appInfo = await client.getApplicationByID(ApplicationID).do();
          } catch (e) {
            debug(lab, {e});
            return undefined;
          }
          const appSt = appInfo['params']['global-state'];
          debug(lab, {appSt});
          return appSt;
        };
        const getGlobalState = async (appSt_g?:any): Promise<GlobalState|undefined> => {
          const appSt = appSt_g || await getAppState();
          if ( !appSt ) { return undefined; }
          const gsbs = readStateBytes('', [], appSt);
          if ( !gsbs ) { return undefined; }
          // `map gvType keyState_gvs` in Haskell
          const gty = T_Tuple([T_UInt, T_UInt, T_Address]);
          // @ts-ignore
          return gty.fromNet(gsbs);
        };
        const canIWin = async (lct:BigNumber): Promise<boolean> => {
          if ( lct.eq(0) ) { return true; }
          const gs = await getGlobalState();
          const r = !gs || lct.eq(gs[1]);
          debug(`canIWin`, { lct, gs, r });
          return r;
        };

        const isin = (await getProvider()).isIsolatedNetwork;
        const isIsolatedNetwork = () => isin;

        return (_theC = { ApplicationID, ctcAddr, Deployer, getLastRound, setLastRound, getLocalState, getAppState, getGlobalState, ensureOptIn, canIWin, isIsolatedNetwork });
      };
    };

    const getState_ = async (getC:any, lookup:((vibna:BigNumber) => AnyALGO_Ty[])): Promise<Array<any>> => {
      const { getAppState, getGlobalState } = await getC();
      const appSt = await getAppState();
      if ( !appSt ) { throw Error(`getState: no appSt`); }
      const gs = await getGlobalState(appSt);
      if ( !gs ) { throw Error(`getState: no gs`); }
      const vvn = recoverSplitBytes('v', stateSize, stateKeys, appSt);
      if ( vvn === undefined ) { throw Error(`getState: no vvn`); }
      const vi = gs[0];
      const vtys = lookup(vi);
      const vty = T_Tuple(vtys);
      const vvs = vty.fromNet(vvn);
      debug(`getState_`, { vvn, vvs });
      return vvs;
    };

    const _setup = (setupArgs: SetupArgs): SetupRes => {
      const { setInfo, getInfo, setTrustedVerifyResult } = setupArgs;

      const didSet = new Signal();
      let fake_info: ContractInfo|undefined = undefined;
      const fake_setInfo = (x:ContractInfo) => {
        fake_info = x;
        didSet.notify();
      };
      const ctorRan = new Signal();
      ctorRan.wait().then(() => {
        if ( fake_info !== undefined ) {
          setInfo(fake_info);
        }
      });
      const fake_getInfo = async (): Promise<ContractInfo> => {
        if ( givenInfoP ) {
          return await getInfo();
        } else {
          await didSet.wait();
          if ( fake_info === undefined ) { throw Error(`impossible fake_info`); }
          return fake_info;
        }
      };

      const eventCache = new EventCache();
      const fake_setupArgs = {
        ...setupArgs,
        getInfo: fake_getInfo,
      };
      const getC = makeGetC(fake_setupArgs, eventCache, () => {});

      // Returns address of a Reach contract
      const getContractAddress = async () => {
        const { ctcAddr } = await getC();
        return T_Address.canonicalize(ctcAddr);
      };
      const getContractInfo = async () => {
        const { ApplicationID } = await getC();
        return ApplicationID;
      };

      const getState = async (vibne:BigNumber, vtys:AnyALGO_Ty[]): Promise<Array<any>> => {
        return await getState_(getC, (vibna:BigNumber) => {
          if ( vibne.eq(vibna) ) { return vtys; }
          throw Error(`Expected state ${vibne}, got ${vibna}`);
        });
      };

      const sendrecv = async (srargs:SendRecvArgs): Promise<Recv> => {
        const { funcNum, evt_cnt, lct, tys, args, pay, out_tys, onlyIf, soloSend, timeoutAt, sim_p } = srargs;
        const isCtor = (funcNum === 0);
        const doRecv = async (didSend: boolean, waitIfNotPresent: boolean): Promise<Recv> => {
          if ( ! didSend && lct.eq(0) ) {
            throw new Error(`API call failed`);
          }
          return await recv({funcNum, evt_cnt, out_tys, didSend, waitIfNotPresent, timeoutAt});
        };
        if ( ! onlyIf ) {
          return await doRecv(false, true);
        }
        const funcName = `m${funcNum}`;
        const dhead = `${label}: sendrecv ${funcName} ${timeoutAt}`;

        const trustedRecv = async (txn:RecvTxn): Promise<Recv> => {
          const didSend = true;
          if ( isCtor ) {
            // If this is the constructor, then we are going to need to notify
            // the ctorRan signal, but we can only do that once the constructor
            // is visible on the indexer, thus we can't rely on a trusted
            // receive. I originally thought we could do this in the
            // background, but the ctorRan signal is representative of what
            // could happen in a real non-test program, so we should really
            // double check with the indexer in a real deployment too.
            return await doRecv(didSend, false);
          }
          return await recvFrom({dhead, out_tys, didSend, funcNum, txn});
        };

        if ( isCtor ) {
          debug(label, 'deploy');
          const compiled = await compileFor(bin);
          const { appApproval, appClear } = compiled;
          const extraPages =
            Math.ceil((appClear.result.length + appApproval.result.length) / MaxAppProgramLen) - 1;

          debug(`deploy`, {extraPages});
          const Deployer = thisAcc.addr;
          const createRes =
            await sign_and_send_sync(
              'ApplicationCreate',
              thisAcc,
              toWTxn(algosdk.makeApplicationCreateTxn(
                Deployer, await getTxnParams(),
                algosdk.OnApplicationComplete.NoOpOC,
                appApproval.result,
                appClear.result,
                appLocalStateNumUInt, appLocalStateNumBytes + mapDataKeys,
                appGlobalStateNumUInt, appGlobalStateNumBytes + stateKeys,
                undefined, undefined, undefined, undefined,
                NOTE_Reach, undefined, undefined, extraPages)));

          const allocRound = createRes['confirmed-round'];
          const ApplicationID = createRes['application-index'];
          if ( ! ApplicationID ) {
            throw Error(`No application-index in ${JSON.stringify(createRes)}`);
          }
          debug(`created`, {ApplicationID});
          const ctcInfo = ApplicationID;
          // We are adding one to the allocRound because we want querying to
          // start at the first place it possibly could, which is going to
          // eliminate the allocation from the event cache.
          // Once we make it so the allocation event is actually needed, then
          // we will modify this.
          setTrustedVerifyResult({ compiled, ApplicationID, Deployer, startRound: allocRound + 1 });
          fake_setInfo(ctcInfo);
        }
        const { ApplicationID, ctcAddr, Deployer, ensureOptIn, canIWin, isIsolatedNetwork } = await getC();

        const [ value, toks ] = pay;
        void(toks); // <-- rely on simulation because of ordering

        debug(dhead, '--- START');

        const [ _svs, msg ] = argsSplit(args, evt_cnt);
        const [ _svs_tys, msg_tys ] = argsSplit(tys, evt_cnt);
        void(_svs); void(_svs_tys);
        const fake_res = {
          didSend: true,
          didTimeout: false,
          data: msg,
          time: bigNumberify(0), // This should not be read.
          secs: bigNumberify(0), // This should not be read.
          value: value,
          from: pks,
          getOutput: (async (o_mode:string, o_lab:string, o_ctc:any, o_val:any): Promise<any> => {
            void(o_mode);
            void(o_lab);
            void(o_ctc);
            return o_val;
          }),
        };
        const sim_r = await sim_p( fake_res );
        debug(dhead , '--- SIMULATE', sim_r);
        if ( isCtor ) {
          sim_r.txns.unshift({
            kind: 'to',
            amt: minimumBalance,
            tok: undefined,
          });
        }
        const { isHalt } = sim_r;

        // Maps
        if ( hasMaps ) { await ensureOptIn(); }
        const { mapRefs } = sim_r;

        while ( true ) {
          const params = await getTxnParams();
          // We add one, because the firstRound field is actually the current
          // round, which we couldn't possibly be in, because it already
          // happened.
          debug(dhead, '--- TIMECHECK', { params, timeoutAt });
          if ( await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, params.firstRound + 1) ) {
            debug(dhead, '--- FAIL/TIMEOUT');
            return await doRecv(false, false);
          }
          if ( ! soloSend && ! await canIWin(lct) ) {
            debug(dhead, `CANNOT WIN`);
            return await doRecv(false, false);
          }

          debug(dhead, '--- ASSEMBLE w/', params);

          const mapAccts: Array<Address> = [ ];
          const recordAccount_ = (addr:Address) => {
            if ( addressEq(thisAcc.addr, addr) ) { return; }
            const addrIdx =
              mapAccts.findIndex((other:Address) => addressEq(other, addr));
            const present = addrIdx !== -1;
            if ( present ) { return; }
            mapAccts.push(addr);
          };
          const recordAccount = (caddr:string) => {
            debug(`recordAccount`, {caddr});
            const addr = cbr2algo_addr(caddr);
            debug(`recordAccount`, {addr});
            recordAccount_(addr);
          };
          mapRefs.forEach(recordAccount);

          const assetsArr: number[] = [];
          const recordAsset = (tok:BigNumber|undefined) => {
            if ( tok ) {
              const tokn = bigNumberToNumber(tok);
              if ( ! assetsArr.includes(tokn) ) {
                assetsArr.push(tokn);
              }
            }
          };
          let extraFees: number = 0;
          let howManyMoreFees: number = 0;
          const txnExtraTxns: Array<Transaction> = [];
          let sim_i = 0;
          const processSimTxn = (t: SimTxn) => {
            let txn;
            if ( t.kind === 'tokenNew' ) {
              processSimTxn({
                kind: 'to',
                amt: minimumBalance,
                tok: undefined,
              });
              howManyMoreFees++; return;
            } else if ( t.kind === 'tokenBurn' ) {
              // There's no burning on Algorand
              return;
            } else if ( t.kind === 'tokenDestroy' ) {
              recordAsset(t.tok);
              howManyMoreFees++; return;
            } else {
              const { tok } = t;
              let amt: BigNumber = bigNumberify(0);
              let from: Address = ctcAddr;
              let to: Address = ctcAddr;
              let closeTo: Address|undefined = undefined;
              if ( t.kind === 'from' ) {
                recordAsset(tok);
                recordAccount(t.to);
                howManyMoreFees++; return;
              } else if ( t.kind === 'init' ) {
                processSimTxn({
                  kind: 'to',
                  amt: minimumBalance,
                  tok: undefined,
                });
                recordAsset(tok);
                howManyMoreFees++; return;
              } else if ( t.kind === 'halt' ) {
                if ( t.tok ) { recordAsset(t.tok); }
                recordAccount_(Deployer);
                howManyMoreFees++; return;
              } else if ( t.kind === 'to' ) {
                from = thisAcc.addr;
                to = ctcAddr;
                amt = t.amt;
              } else {
                assert(false, 'sim txn kind');
              }
              if ( amt.eq(0) ) { return; }
              txn = makeTransferTxn(from, to, amt, tok, params, closeTo, sim_i++);
            }
            extraFees += txn.fee;
            txn.fee = 0;
            txnExtraTxns.push(txn);
          };
          sim_r.txns.forEach(processSimTxn);
          debug(dhead, 'txnExtraTxns', txnExtraTxns);
          debug(dhead, {howManyMoreFees, extraFees});
          extraFees += MinTxnFee * howManyMoreFees;
          debug(dhead, {extraFees});

          debug(dhead, 'MAP', { mapAccts });
          if ( mapAccts.length > MaxAppTxnAccounts ) {
            throw Error(`Application references too many local state cells in one step. Reach should catch this problem statically.`);
          }
          const mapAcctsVal =
            (mapAccts.length === 0) ? undefined : mapAccts;

          const assetsVal: number[]|undefined =
            (assetsArr.length === 0) ? undefined : assetsArr;
          debug(dhead, {assetsArr, assetsVal});

          const actual_args = [ lct, msg ];
          const actual_tys = [ T_UInt, T_Tuple(msg_tys) ];
          debug(dhead, '--- ARGS =', actual_args);

          const safe_args: Array<NV> = actual_args.map(
            // @ts-ignore
            (m, i) => actual_tys[i].toNet(m));
          safe_args.unshift(new Uint8Array([funcNum]));
          safe_args.forEach((x) => {
            if (! ( x instanceof Uint8Array ) ) {
              // The types say this is impossible now,
              // but we'll leave it in for a while just in case...
              throw Error(`expect safe program argument, got ${JSON.stringify(x)}`);
            }
          });
          debug(dhead, '--- PREPARE:', safe_args.map(ui8h));

          const whichAppl =
            isHalt ?
            // We are treating it like any party can delete the application, but the docs say it may only be possible for the creator. The code appears to not care: https://github.com/algorand/go-algorand/blob/0e9cc6b0c2ddc43c3cfa751d61c1321d8707c0da/ledger/apply/application.go#L589
            algosdk.makeApplicationDeleteTxn :
            algosdk.makeApplicationNoOpTxn;
          const txnAppl =
            whichAppl(
              thisAcc.addr, params, ApplicationID, safe_args,
              mapAcctsVal, undefined, assetsVal, NOTE_Reach);
          txnAppl.fee += extraFees;
          const rtxns = [ ...txnExtraTxns, txnAppl ];
          debug(dhead, `assigning`, { rtxns });
          algosdk.assignGroupID(rtxns);
          const wtxns = rtxns.map(toWTxn);

          debug(dhead, 'signing', { wtxns });
          let res;
          try {
            res = await signSendAndConfirm( thisAcc, wtxns );
          } catch (e:any) {
            const es = ( e.type === 'sendRawTransaction' ) ?
              format_failed_request(e?.e) : e;
            debug(dhead, '--- FAIL:', es);

            if ( ! soloSend ) {
              // If there is no soloSend, then someone else "won", so let's
              // listen for their message
              debug(dhead, 'LOST');
              return await doRecv(false, false);
            }

            if ( timeoutAt ) {
              // If there can be a timeout, then keep waiting for it
              debug(dhead, `CONTINUE`);
              continue;
            } else {
              // Otherwise, something bad is happening
              throw Error(`${label} failed to call ${funcName}: ${JSON.stringify(es)}`);
            }
          }

          debug(dhead, 'SUCCESS', res);
          return await trustedRecv(res);
        }
      };

      type RecvFromArgs = {
        dhead: any,
        out_tys: Array<ConnectorTy>,
        didSend: boolean,
        funcNum: number,
        txn: RecvTxn,
      };
      const recvFrom = async (rfargs:RecvFromArgs): Promise<Recv> => {
        const { dhead, out_tys, didSend, txn } = rfargs;
        const { getLastRound, setLastRound } = await getC();
        debug(dhead, '--- txn =', txn);
        const theRound = txn['confirmed-round'];
        // const theSecs = txn['round-time'];
        // ^ The contract actually uses `global LatestTimestamp` which is the
        // time of the PREVIOUS round.
        // ^ Also, this field is only available from the indexer
        const theSecs = await retryLoop([dhead, 'getTimeSecs'], () => getTimeSecs(bigNumberify(theRound - 0)));
        // ^ XXX it would be nice if Reach could support variables bound to
        // promises and then we wouldn't need to wait here.

        // XXX need to move this to a log
        const ctc_args_all = txn['application-args'];
        debug(dhead, {ctc_args_all});
        const argMsg = 2; // from ALGO.hs
        const ctc_args_s: string = ctc_args_all[argMsg];

        debug(dhead, 'out_tys', out_tys.map((x) => x.name));
        const msgTy = T_Tuple(out_tys);
        const ctc_args = msgTy.fromNet(reNetify(ctc_args_s));
        debug(dhead, {ctc_args});

        const fromAddr = txn['sender'];
        const from = T_Address.canonicalize({addr: fromAddr});
        debug(dhead, { from, fromAddr });

        const oldLastRound = getLastRound();
        setLastRound(theRound);
        debug(dhead, { oldLastRound, theRound });
        const getOutput = async (o_mode:string, o_lab:string, o_ctc:any, o_val:any): Promise<any> => {
          debug(`getOutput`, {o_mode, o_lab, o_ctc, o_val});
          const f_ctc = T_Tuple([T_UInt, o_ctc]);
          for ( const l of txn['logs'] ) {
            const lb = reNetify(l);
            const ln = T_UInt.fromNet(lb);
            const ls = `v${ln}`;
            debug(`getOutput`, {l, lb, ln, ls});
            if ( ls === o_lab ) {
              const ld = f_ctc.fromNet(lb);
              const o = ld[1];
              debug(`getOutput`, {ld, o});
              return o;
            }
          }
          throw Error(`no log for ${o_lab}`);
        };

        return {
          didSend,
          didTimeout: false,
          data: ctc_args,
          time: bigNumberify(getLastRound()),
          secs: bigNumberify(theSecs),
          from, getOutput,
        };
      };

      const recv = async (rargs:RecvArgs): Promise<Recv> => {
        const { funcNum, out_tys, didSend, waitIfNotPresent, timeoutAt } = rargs;
        const isCtor = (funcNum == 0);
        const fromBlock_summand = isCtor ? 0 : 1;

        const funcName = `m${funcNum}`;
        const dhead = `${label}: ${label} recv ${funcName} ${timeoutAt}`;
        debug(dhead, '--- START');

        const { ApplicationID, getLastRound, isIsolatedNetwork } = await getC();

        while ( true ) {
          const correctStep = makeIsMethod(funcNum);
          const minRound = getLastRound() + fromBlock_summand;
          const res = await eventCache.query(dhead, ApplicationID, { minRound, timeoutAt }, correctStep);
          debug(`EventCache res: `, res);
          if ( ! res.succ ) {
            const currentRound = res.round;
            debug(dhead, 'TIMECHECK', {timeoutAt, minRound, currentRound});
            if ( await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, currentRound+1) ) {
              debug(dhead, 'TIMEOUT');
              return { didTimeout: true };
            }
            if ( waitIfNotPresent ) {
              await waitUntilTime(bigNumberify(currentRound + 1));
            } else {
              await indexer_statusAfterBlock(currentRound + 1);
            }
            continue;
          }
          if ( isCtor ) {
            ctorRan.notify();
          }
          const txn = indexerTxn2RecvTxn(res.txn);
          return await recvFrom({dhead, out_tys, didSend, funcNum, txn});
        }
      };

      return { getContractInfo, getContractAddress, getState, sendrecv, recv };
    };

    const readStateBytes = (prefix:string, key:number[], src:any): any => {
      debug({prefix, key});
      const ik = base64ify(new Uint8Array(key));
      debug({ik});
      const ste = src.find((x:any) => x.key === ik);
      debug({ste});
      if ( ste === undefined ) { return []; };
      const st = ste.value;
      debug({st});
      if ( st.bytes === undefined ) { return []; };
      const bsi = base64ToUI8A(st.bytes);
      debug({bsi});
      return bsi;
    };
    const recoverSplitBytes = (prefix:string, size:number, howMany:number, src:any): any => {
      const bs = new Uint8Array(size);
      let offset = 0;
      for ( let i = 0; i < howMany; i++ ) {
        const bsi = readStateBytes(prefix, [i], src);
        if ( bsi.length == 0 ) {
          return undefined;
        }
        bs.set(bsi, offset);
        offset += bsi.length;
      }
      return bs;
    };
    const setupView = (setupViewArgs: SetupViewArgs) => {
      const eventCache = new EventCache();
      const getC = makeGetC(setupViewArgs, eventCache, () => {});
      const viewLib: IViewLib = {
        viewMapRef: async (mapi: number, a:any): Promise<any> => {
          const { getLocalState } = await getC();
          debug('viewMapRef', { mapi, a });
          const ls = await getLocalState(cbr2algo_addr(a));
          assert(ls !== undefined, 'viewMapRef ls undefined');
          debug('viewMapRef', { ls });
          const mbs = recoverSplitBytes('m', mapDataSize, mapDataKeys, ls);
          debug('viewMapRef', { mbs });
          const md = mapDataTy.fromNet(mbs);
          debug('viewMapRef', { md });
          // @ts-ignore
          const mr = md[mapi];
          assert(mr !== undefined, 'viewMapRef mr undefined');
          return mr;
        },
      };
      const getView1 = (vs:BackendViewsInfo, v:string, k:string|undefined, vim: BackendViewInfo, isSafe = true) =>
        async (...args: any[]): Promise<any> => {
          debug('getView1', v, k, args);
          const { decode } = vim;
          try {
            let vi = 0;
            const vvs = await getState_(getC, (vibna:BigNumber) => {
              vi = bigNumberToNumber(vibna);
              const vtys = vs[vi];
              if ( ! vtys ) { throw Error(`no views for state ${vibna}`); }
              return vtys;
            });
            const vres = await decode(vi, vvs, args);
            debug({vres});
            return isSafe ? ['Some', vres] : vres;
          } catch (e) {
            debug(`getView1`, v, k, 'error', e);
            if (isSafe) {
              return ['None', null];
            } else {
              throw Error(`View ${v}.${k} is not set.`);
            }
          }
      };
      return { getView1, viewLib };
    };

    const setupEvents = (a: SetupEventArgs) => {
      const eventCache = new EventCache();
      let time = bigNumberify(0);
      const getC = makeGetC(a, eventCache, (cb: number) => {
        time = bigNumberify(cb);
      });
      const createEventStream = (event: string, tys: AnyALGO_Ty[]) => {
        let logIndex: any = {};
        let sig = `${event}(${tys.map(ty => ty.netName).join(',')})`;
        debug(`createEventStream signature`, sig);
        let hashPrefix = sha512_256(sig).substring(0, 4);
        let base64Hash = replaceAll(base64ify(hashPrefix) as string, '=', '');
        debug(`createEventStream hash`, base64Hash);
        let lastLog: any = undefined;

        const seek = (t: Time) => {
          debug("EventStream::seek", t);
          time = t;
          logIndex[time.toNumber()] = 0;
        }

        const next = async () => {
          let dhead = "EventStream::next";
          const { ApplicationID } = await getC();
          const pred = (txn: any) => {
            const round = txn['confirmed-round'];
            const logIdx = logIndex[round] || 0;
            const logs: string[] = (txn['logs'] || []).slice(logIdx);
            const good = logs.some((log) => log.startsWith(base64Hash));
            return good;
          };
          let res: QueryResult = { succ: false, round: 0  };
          while (!res.succ) {
            res = await eventCache.query(dhead, ApplicationID, { minRound: time.toNumber(), isEventStream: true }, pred);
            if (!res.succ) { await Timeout.set(5000); }
          }
          const round = res.txn['confirmed-round'];
          const logIdx = logIndex[round] || 0;
          const logs = res.txn.logs.slice(logIdx);
          let log = logs.find((l: string, idx: number) => {
            const matches = l.startsWith(base64Hash);
            if (matches) { logIndex[round] = logIdx + idx + 1; }
            return matches;
          });
          // @ts-ignore
          const parsedLog = T_Tuple([T_Bytes(4)].concat(tys)).fromNet(reNetify(log));
          const blockTime = bigNumberify(round);
          time = blockTime;
          debug(dhead + ` parsed log`, parsedLog, blockTime);
          parsedLog.shift(); // Remove tag
          lastLog = { when: blockTime, what: parsedLog };
          return lastLog;
        }

        const seekNow = async () => {
          time = await getNetworkTime();
        }

        const lastTime = async () => {
          const dhead = "EventStream::lastTime";
          debug(dhead, time);
          return lastLog?.when;
        }

        const monitor = async (onEvent: (x: any) => void) => {
          while (true) {
            onEvent(await next());
          }
        }

        return { lastTime, seek, seekNow, monitor, next };
      };
      return { createEventStream };
    }

    return stdContract({ bin, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, setupEvents, _setup, givenInfoP });
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  }

  const me_na = { networkAccount };
  const tokenAccepted = async (token:Token): Promise<boolean> => {
    debug(`tokenAccepted`, token);
    // @ts-ignore
    const r = await balanceOfM(me_na, token);
    return ( r !== false );
  };
  const tokenAccept = async (token:Token): Promise<void> => {
    if ( ! (await tokenAccepted(token)) ) {
      debug(`tokenAccept`, token);
      // @ts-ignore
      await transfer(me_na, me_na, 0, token);
    }
  };
  const tokenMetadata = async (token:Token): Promise<any> => {
    debug(`tokenMetadata`, token);
    const client = await getAlgodClient();
    const tokenRes = await client.getAssetByID(bigNumberToNumber(token)).do();
    debug({tokenRes});
    const tokenInfo = tokenRes['params'];
    debug({tokenInfo});
    const p_t = (t:AnyALGO_Ty, x:string): any =>
      x ? t.fromNet(reNetify(x)) : undefined;
    const p = (n:number, x:string): any =>
      p_t(T_Bytes(n), x);
    // XXX share these numbers with hs and ethlike(?)
    const name = p(32, tokenInfo['name-b64']);
    const symbol = p(8, tokenInfo['unit-name-b64']);
    const url = p(96, tokenInfo['url-b64']);
    const metadata =
      (() => {
        const mh = tokenInfo['metadata-hash'];
        try {
          return p(32, mh);
        } catch (e:any) {
          debug(`tokenMetadata metadata-hash`, `${e}`);
          return p_t(T_Digest, mh);
        }
    })();
    const supply = bigNumberify(tokenInfo['total']);
    const decimals = bigNumberify(tokenInfo['decimals']);
    return { name, symbol, url, metadata, supply, decimals };
  };

  return stdAccount({ networkAccount, getAddress: selfAddress, stdlib, setDebugLabel, tokenAccepted, tokenAccept, tokenMetadata, contract });
};

const balanceOfM = async (acc: Account, token: Token|false = false): Promise<BigNumber|false> => {
  const addr = extractAddr(acc);
  const client = await getAlgodClient();
  const info = await client.accountInformation(addr).do();
  debug(`balanceOf`, info);
  if ( ! token ) {
    return bigNumberify(info.amount);
  } else {
    for ( const ai of info.assets ) {
      if ( bigNumberify(token).eq(ai['asset-id']) ) {
        return ai['amount'];
      }
    }
    return false;
  }
};

export const balanceOf = async (acc: Account, token: Token|false = false): Promise<BigNumber> => {
  const r = await balanceOfM(acc, token);
  if ( r === false ) {
    return bigNumberify(0);
  }
  return r;
};

export const createAccount = async (): Promise<Account> => {
  const networkAccount = algosdk.generateAccount();
  return await connectAccount(networkAccount);
};

export const canFundFromFaucet = async (): Promise<boolean> => {
  const faucet = await getFaucet();
  const algodClient = await getAlgodClient();
  debug('ALGO:canFundFromFaucet: check genesis');
  const txnParams = await algodClient.getTransactionParams().do();
  const act = txnParams.genesisID;
  const exp = 'devnet-v1';
  if (act !== exp) {
    debug(`ALGO:canFundFromFaucet: expected '${exp}' !== actual '${act}'`);
    return false;
  }
  debug('ALGO:canFundFromFaucet: check balance');
  const fbal = await balanceOf(faucet);
  debug(`ALGO:canFundFromFaucet: faucet balance = ${formatCurrency(fbal, 4)} ${standardUnit}`);
  return gt(fbal, 0);
};

export const fundFromFaucet = async (account: Account, value: any) => {
  const faucet = await getFaucet();
  debug('fundFromFaucet');
  const tag = Math.round(Math.random() * (2 ** 32));
  await transfer(faucet, account, value, undefined, tag);
};

export const newTestAccount = async (startingBalance: any) => {
  const account = await createAccount();
  await fundFromFaucet(account, startingBalance);
  return account;
};

export const newTestAccounts = make_newTestAccounts(newTestAccount);

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'ALGO';

/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'μALGO';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100000000'
 */
export function parseCurrency(amt: CurrencyAmount): BigNumber {
  // @ts-ignore
  const numericAmt: number =
    isBigNumber(amt) ? amt.toNumber()
    : typeof amt === 'string' ? parseFloat(amt)
    : typeof amt === 'bigint' ? Number(amt)
    : amt;
  return bigNumberify(algosdk.algosToMicroalgos(numericAmt));
}

export const minimumBalance: BigNumber =
  bigNumberify(MinBalance);

// lol I am not importing leftpad for this
/** @example lpad('asdf', '0', 6); // => '00asdf' */
function lpad(str: string, padChar: string, nChars: number) {
  const padding = padChar.repeat(Math.max(nChars - str.length, 0));
  return padding + str;
}

/** @example rdrop('asfdfff', 'f'); // => 'asfd' */
function rdrop(str: string, char: string) {
  while (str[str.length - 1] === char) {
    str = str.slice(0, str.length - 1);
  }
  return str;
}

/** @example ldrop('007', '0'); // => '7' */
function ldrop(str: string, char: string) {
  while (str[0] === char) {
    str = str.slice(1);
  }
  return str;
}

/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeros will be omitted. Excess decimal places will be truncated (not rounded).
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 * @example  formatCurrency(bigNumberify('9999998799987000')); // => '9999998799.987'
 */
export function formatCurrency(amt: any, decimals: number = 6): string {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  const amtStr = bigNumberify(amt).toString();
  const splitAt = Math.max(amtStr.length - 6, 0);
  const lPredropped = amtStr.slice(0, splitAt);
  const l = ldrop(lPredropped, '0') || '0';
  if (decimals === 0) { return l; }

  const rPre = lpad(amtStr.slice(splitAt), '0', 6);
  const rSliced = rPre.slice(0, decimals);
  const r = rdrop(rSliced, '0');

  return r ? `${l}.${r}` : l;
}

export async function getDefaultAccount(): Promise<Account> {
  const addr = await (await getProvider()).getDefaultAddress();
  return await connectAccount({ addr });
}

/**
 * @param mnemonic 25 words, space-separated
 */
export const newAccountFromMnemonic = async (mnemonic: string): Promise<Account> => {
  return await connectAccount(algosdk.mnemonicToSecretKey(mnemonic));
};

/**
 * @param secret a Uint8Array, or its hex string representation
 */
export const newAccountFromSecret = async (secret: string | SecretKey): Promise<Account> => {
  const sk = ethers.utils.arrayify(secret);
  const mnemonic = algosdk.secretKeyToMnemonic(sk);
  return await newAccountFromMnemonic(mnemonic);
};

export const getNetworkTime = async (): Promise<BigNumber> => {
  const indexer = await getIndexer();
  const hc = await indexer.makeHealthCheck().do();
  return bigNumberify(hc['round']);
};
const getTimeSecs = async (now_bn: BigNumber): Promise<BigNumber> => {
  const now = bigNumberToNumber(now_bn);
  try {
    const client = await getAlgodClient();
    const binfo = await client.block(now).do();
    debug(`getTimeSecs`, `node`, binfo);
    return bigNumberify(binfo.block.ts);
  } catch (e:any) {
    debug(`getTimeSecs`, `node failed`, e);
    const indexer = await getIndexer();
    const info = await indexer.lookupBlock(now).do();
    debug(`getTimeSecs`, `indexer`, info);
    return bigNumberify(info['timestamp']);
  }
};
export const getNetworkSecs = async (): Promise<BigNumber> =>
  await getTimeSecs(await getNetworkTime());

const stepTime = async (target: BigNumber): Promise<BigNumber> => {
  if ( (await getProvider()).isIsolatedNetwork ) {
    await fundFromFaucet(await getFaucet(), 0);
  }
  return await indexer_statusAfterBlock(bigNumberToNumber(target));
};
export const waitUntilTime = make_waitUntilX('time', getNetworkTime, stepTime);

const stepSecs = async (target: BigNumber): Promise<BigNumber> => {
  void(target);
  const now = await stepTime((await getNetworkTime()).add(1));
  return await getTimeSecs(now);
};
export const waitUntilSecs = make_waitUntilX('secs', getNetworkSecs, stepSecs);

export const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  return await waitUntilTime(now.add(delta), onProgress);
};

const appLocalStateNumUInt = 0;
const appLocalStateNumBytes = 0;
const appGlobalStateNumUInt = 0;
const appGlobalStateNumBytes = 1;

type VerifyResult = {
  compiled: CompiledBackend,
  ApplicationID: number,
  Deployer: Address,
  startRound: number,
};

export const verifyContract = async (info: ContractInfo, bin: Backend): Promise<VerifyResult> => {
  return verifyContract_('', info, bin, new EventCache());
}

const verifyContract_ = async (label:string, info: ContractInfo, bin: Backend, eventCache: EventCache): Promise<VerifyResult> => {
  const compiled = await compileFor(bin);
  const ApplicationID = info;
  const { appApproval, appClear } = compiled;
  const { mapDataKeys, stateKeys } = bin._Connectors.ALGO;

  let dhead = `${label}: verifyContract`;

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`${dhead} failed: ${msg}`);
    }
  };
  const chkeq = (a: any, e:any, msg:string) => {
    const as = JSON.stringify(a);
    const es = JSON.stringify(e);
    chk(as === es, `${msg}: expected ${es}, got ${as}`);
  };
  const fmtp = (x: CompileResultBytes) => uint8ArrayToStr(x.result, 'base64');

  const client = await getAlgodClient();
  let appInfo; let err;
  try {
    appInfo = await client.getApplicationByID(ApplicationID).do();
  } catch (e) {
    err = e;
  }
  if ( appInfo === undefined ) {
    throw Error(`${dhead} failed: failed to lookup application (${ApplicationID}): ${JSON.stringify(err)}`);
  }
  const appInfo_p = appInfo['params'];
  debug(dhead, {appInfo_p});
  chk(appInfo_p, `Cannot lookup ApplicationId`);
  chkeq(appInfo_p['approval-program'], fmtp(appApproval), `Approval program does not match Reach backend`);
  chkeq(appInfo_p['clear-state-program'], fmtp(appClear), `ClearState program does not match Reach backend`);
  const Deployer = appInfo_p['creator'];

  const appInfo_LocalState = appInfo_p['local-state-schema'];
  chkeq(appInfo_LocalState['num-byte-slice'], appLocalStateNumBytes + mapDataKeys, `Num of byte-slices in local state schema does not match Reach backend`);
  chkeq(appInfo_LocalState['num-uint'], appLocalStateNumUInt, `Num of uints in local state schema does not match Reach backend`);

  const appInfo_GlobalState = appInfo_p['global-state-schema'];
  chkeq(appInfo_GlobalState['num-byte-slice'], appGlobalStateNumBytes + stateKeys, `Num of byte-slices in global state schema does not match Reach backend`);
  chkeq(appInfo_GlobalState['num-uint'], appGlobalStateNumUInt, `Num of uints in global state schema does not match Reach backend`);

  const indexer = await getIndexer();
  const ilq = indexer.lookupApplications(ApplicationID).includeAll();
  const ilr = await doQuery_(`${dhead} app lookup`, ilq, true);
  debug(dhead, {ilr});
  const appInfo_i = ilr.application;
  debug(dhead, {appInfo_i});
  chkeq(appInfo_i['deleted'], false, `Application must not be deleted`);
  // First, we learn from the indexer when it was made
  const allocRound = appInfo_i['created-at-round'];

  // Next, we check that it was created with this program and wasn't created
  // with a different program first (which could have modified the state)
  const iar = await eventCache.query(dhead, ApplicationID, { specRound: allocRound }, (_: any) => true);
  // @ts-ignore
  const iat = iar.txn;
  chk(iat, `Cannot query for allocation transaction`);
  debug({iat});
  const iatat = iat['application-transaction'];
  debug({iatat});
  chkeq(iatat['approval-program'], appInfo_p['approval-program'], `ApprovalProgram unchanged since creation`);
  chkeq(iatat['clear-state-program'], appInfo_p['clear-state-program'], `ClearStateProgram unchanged since creation`);

  // Next, we wait for the constructor call
  // XXX maybe don't care about this
  const isCtor = makeIsMethod(0);
  const icr = await eventCache.query(`${dhead} ctor`, ApplicationID, { minRound: 0 }, isCtor);
  debug({icr});
  // @ts-ignore
  const ict = icr.txn;
  chk(ict, `Cannot query for constructor transaction`);
  debug({ict});
  const ctorRound = ict['confirmed-round']

  return { compiled, ApplicationID, Deployer, startRound: ctorRound };
};

/**
 * Formats an account's address in the way users expect to see it.
 * @param acc Account, NetworkAccount, base32-encoded address, or hex-encoded address
 * @returns the address formatted as a base32-encoded string with checksum
 */
export function formatAddress(acc: string|NetworkAccount|Account): string {
  return addressFromHex(T_Address.canonicalize(acc));
}

export async function launchToken (accCreator:Account, name:string, sym:string, opts:any = {}) {
  debug(`Launching token, ${name} (${sym})`);
  const addr = (acc:Account) => acc.networkAccount.addr;
  const caddr = addr(accCreator);
  const zaddr = caddr;
  // ^ XXX should be nothing; docs say can be "", but doesn't actually work
  const algod = await getAlgodClient();
  const dotxn = async (mktxn:(params:TxnParams) => Transaction, acc:Account = accCreator) => {
    const sk = acc.networkAccount.sk;
    if ( ! sk ) {
      throw new Error(`can only launchToken with account with secret key`);
    }
    const params = await getTxnParams();
    const t = mktxn(params);
    const s = t.signTxn(sk);
    const r = (await algod.sendRawTransaction(s).do());
    await waitForConfirmation(r.txId);
    return await algod.pendingTransactionInformation(r.txId).do();
  };
  const supply = (opts.supply && bigNumberify(opts.supply)) || bigNumberify(2).pow(64).sub(1);
  const decimals = opts.decimals !== undefined ? opts.decimals : 6;
  const ctxn_p = await dotxn(
    (params:TxnParams) =>
    algosdk.makeAssetCreateTxnWithSuggestedParams(
      caddr, undefined, bigNumberToBigInt(supply), decimals,
      false, zaddr, zaddr, zaddr, zaddr,
      sym, name, '', '', params,
    ));
  const id = ctxn_p["asset-index"];
  debug(`${sym}: asset is ${id}`);

  const mint = async (accTo:Account, amt:any) => {
    debug(`${sym}: transferring ${amt} ${sym} for ${addr(accTo)}`);
    await transfer(accCreator, accTo, amt, id);
  };
  const optOut = async (accFrom:Account, accTo:Account = accCreator) => {
    await dotxn(
      (params) =>
      algosdk.makeAssetTransferTxnWithSuggestedParams(
        addr(accFrom), addr(accTo), addr(accTo), undefined,
        0, undefined, id, params
      ), accFrom);
  };
  return { name, sym, id, mint, optOut };
};

export const reachStdlib = stdlib;
