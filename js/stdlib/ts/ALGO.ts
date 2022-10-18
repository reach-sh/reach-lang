import {
  Stdlib_User,
} from './interfaces';
import algosdk from 'algosdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import buffer from 'buffer';
import type MyAlgoConnect from '@randlabs/myalgo-connect';
import type {
  Transaction,
  EncodedTransaction,
  SuggestedParams,
} from 'algosdk'; // =>
import type {
  ARC11_Wallet,
  WalletTransaction,
  EnableNetworkResult,
  EnableAccountsResult,
  EnableOpts,
  EnableResult,
  EnableAccountsOpts,
} from './ALGO_ARC11'; // =>
import type { BaseHTTPClient } from 'algosdk';
import * as RHC from './ALGO_ReachHTTPClient';
// @ts-ignore // XXX Dan FIXME pls
import * as UTBC from './ALGO_UTBC';

const {Buffer} = buffer;

import {
  VERSION
} from './version';
import {
  OnProgress,
  apiStateMismatchError,
  IViewLib, IBackend, IBackendViewInfo, IBackendViewsInfo,
  IRecvArgs, ISendRecvArgs,
  IAccount, IContract, IRecv,
  ISetupArgs, ISetupViewArgs, ISetupRes,
  // ISimRes,
  ISimTxn, ISimRemote,
  stdContract, stdVerifyContract,
  stdABIFilter,
  stdAccount,
  stdAccount_unsupported,
  debug, envDefault,
  argsSplit,
  makeRandom,
  replaceableThunk,
  ensureConnectorAvailable,
  make_newTestAccounts,
  make_waitUntilX,
  checkTimeout,
  truthyEnv,
  Lock,
  retryLoop,
  ISetupEventArgs,
  IEventQueue,
  EQGetTxnsR,
  makeEventQueue,
  makeEventStream,
  TokenMetadata,
  LaunchTokenOpts,
  NotifyComplete,
  makeSigningMonitor,
  j2sf,
  j2s,
  hideWarnings,
  hasProp,
  makeParseCurrency,
  TransferOpts,
  stdlibShared,
  protectMnemonic,
  protectSecretKey,
  SecretKeyInput,
  SecretKey,
  Mnemonic,
  mkGetEventTys,
} from './shared_impl';
import {
  bigNumberify,
  bigNumberToNumber,
  bigNumberToBigInt,
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
  bytestringyNet,
} from './ALGO_compiled';
export type { Token } from './ALGO_compiled';
import {
  simTokenAccepted_
} from './shared_backend';
import type { MapRefT, MaybeRep } from './shared_backend'; // =>
import { window, process } from './shim';
import { sha512_256 } from 'js-sha512';
import * as shared_user from './shared_user';
import * as shared_impl from './shared_impl';

// Type Definitions
type BigNumber = ethers.BigNumber;

type AnyALGO_Ty = ALGO_Ty<CBR_Val>;
type ConnectorTy= AnyALGO_Ty;
export type Ty = AnyALGO_Ty;
// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX Copy/pasted type defs from types/algosdk
// This is so that this module can be exported without our custom types/algosdk
// The unused ones are commented out
export type Address = string
// type RawAddress = Uint8Array;

type TxnParamsRaw = {
  flatFee?: boolean,
  fee: bigint,
  firstRound: bigint,
  lastRound: bigint,
  genesisID: string,
  genesisHash: string,
}
// XXX Remove once algosdk support bigints
type TxnParams = SuggestedParams;
type RecvTxn = {
  'confirmed-round': bigint,
  'created-asset-index'?: bigint,
  'created-application-index'?: bigint,
  'created-companion-application-index'?: bigint,
  'application-index'?: bigint,
  'application-args': Array<string>,
  'sender': Address,
  'logs': Array<string>,
  'approval-program'?: string,
  'clear-state-program'?: string,
};
type TxId = string;
type ApiCall<T> = {
  query: object,
  do: () => Promise<T>,
};

export type NetworkAccount = {
  addr: Address,
  sk?: SecretKey
};
export type ProviderName = string;
export interface BasicProvider {
  algod_bc: BaseHTTPClient,
  indexer_bc: BaseHTTPClient,
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
}
export interface Provider extends BasicProvider {
  nodeWriteOnly: boolean,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: object) => Promise<unknown>,
};
export interface ProviderEnv {
  ALGO_SERVER: string
  ALGO_PORT: string
  ALGO_TOKEN_HEADER: string
  ALGO_TOKEN: string
  ALGO_INDEXER_SERVER: string
  ALGO_INDEXER_PORT: string
  ALGO_INDEXER_TOKEN_HEADER: string
  ALGO_INDEXER_TOKEN: string
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
  ALGO_NODE_WRITE_ONLY: string // preferably: 'yes' | 'no'
};
const defaultALGO_TOKEN_HEADER = 'X-Algo-API-Token';
const defaultALGO_INDEXER_TOKEN_HEADER = 'X-Indexer-API-Token';

const reachBackendVersion = 25;
const reachAlgoBackendVersion = 11;
export type Backend = IBackend<AnyALGO_Ty> & {_Connectors: {ALGO: {
  version: number,
  ABI: any,
  appApproval: string,
  appClear: string,
  companionInfo: {[key: string]: number}|null,
  extraPages: number,
  stateSize: number,
  stateKeys: number,
  mapDataSize: number,
  mapDataKeys: number,
  unsupported: Array<string>,
  warnings: Array<string>,
}}};
type BackendViewsInfo = IBackendViewsInfo<AnyALGO_Ty>;
type BackendViewInfo = IBackendViewInfo<AnyALGO_Ty>;

export type ContractInfo = BigNumber;
type SendRecvArgs = ISendRecvArgs<Address, Token, AnyALGO_Ty, ContractInfo>;
type RecvArgs = IRecvArgs<AnyALGO_Ty>;
type Recv = IRecv<Address>
export type Contract = IContract<ContractInfo, Address, Token, AnyALGO_Ty>;
export type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
type SimRemote = ISimRemote<Token, ContractInfo>
type SimTxn = ISimTxn<Token, ContractInfo>

type VerifyResult = {
  ApplicationID: BigNumber,
  Deployer: Address,
};
type SetupArgs = ISetupArgs<ContractInfo, VerifyResult>;
type SetupViewArgs = ISetupViewArgs<ContractInfo, VerifyResult>;
type SetupEventArgs = ISetupEventArgs<ContractInfo, VerifyResult>;
type SetupRes = ISetupRes<ContractInfo, Address, Token, AnyALGO_Ty>;

type AssetHolding = {
  'asset-id': bigint,
  'amount': bigint,
};
type AppStateVal = {
  'bytes'?: string,
};
type AppStateKV = {
  'key': string,
  'value': AppStateVal,
};
type AppStateKVs = Array<AppStateKV>;
type AppState = {
  'id': bigint,
  'key-value': AppStateKVs,
};
type AppSchema = {
  "num-byte-slice": bigint,
  "num-uint": bigint
}
type AccountInfo = {
  'amount': bigint,
  'assets'?: Array<AssetHolding>,
  'apps-local-state'?: Array<AppState>,
  'apps-total-schema'?: AppSchema,
  'created-apps'?: Array<AppInfo>
};
type AccountAssetInfo = {
  'asset-holding'?: AssetHolding
};
type AccountApplicationInfo = {
  'app-local-state'?: AppState,
};
type IndexerAccountInfoRes = {
  'current-round': bigint,
  'account': AccountInfo,
};

type AppStateSchema = {
  'num-uint': bigint,
  'num-byte-slice': bigint,
};
type AppInfo = {
  'id': bigint,
  'created-at-round': bigint,
  'deleted': boolean,
  'params': {
    'creator': string,
    'approval-program': string,
    'clear-state-program': string,
    'global-state': AppStateKVs,
    'extra-program-pages': bigint,
    'local-state-schema': AppStateSchema,
    'global-state-schema': AppStateSchema,
  },
};
type IndexerAppInfoRes = {
  'current-round': bigint,
  'application': AppInfo,
};
type IndexerAccountAppLocalStatesRes = {
  'current-round': bigint,
  'apps-local-states': Array<AppState>,
};
type IndexerAccountAssetsRes = {
  'current-round': bigint,
  'assets': Array<AssetHolding>,
};

type AssetInfo = {
  'index': bigint,
  'params': {
    'clawback': string,
    'creator': string,
    'decimals': bigint,
    'default-frozen': boolean,
    'freeze': string,
    'manager': string,
    'metadata-hash': string,
    'name': string,
    'name-b64': string,
    'reserve': string,
    'total': bigint,
    'unit-name': string,
    'unit-name-b64': string,
    'url': string,
    'url-b64': string,
  },
};
type IndexerAssetInfoRes = {
  'current-round': bigint,
  'asset': AssetInfo,
};

type OrExn<X> = { val: X } | {exn:any};
type IndexerAppTxn = {
  'approval-program'?: string,
  'clear-state-program'?: string,
  'application-id'?: bigint,
  'application-args'?: Array<string>,
  'on-completion'?: string,
  'local-state-schema'?: AppStateSchema,
  'global-state-schema'?: AppStateSchema,
  'extra-program-pages'?: bigint,
};
type IndexerTxn = {
  'confirmed-round': bigint,
  'sender': Address,
  'created-asset-index'?: bigint,
  'created-application-index'?: bigint,
  'application-transaction'?: IndexerAppTxn,
  'logs'?: Array<string>,
  'inner-txns'?: Array<IndexerTxn>,
  'tx-type': string,
};
type IndexerQuery1Res = {
  'current-round': bigint,
  'transaction': IndexerTxn,
};
type IndexerQueryMRes = {
  'current-round': bigint,
  'transactions': Array<IndexerTxn>,
};
type AlgodTxn = {
  'asset-index'?: bigint,
  'application-index'?: bigint,
  'confirmed-round'?: bigint,
  'inner-txns'?: Array<AlgodTxn>,
  'logs'?: Array<string>,
  'txn': {
    'sig': Uint8Array,
    'txn': EncodedTransaction,
  },
  'pool-error': string,
};

interface ALGOHacks {
  signSendAndConfirm: any,
  toWTxn: any,
  algosdk: any,
  getTxnParams: any,
  MinTxnFee: any,
  makeTransferTxn: any,
  setFaucet: any,
};
interface ALGOStdlib extends Stdlib_User<Provider, ProviderEnv, ProviderName, Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Contract, Account>, ALGOHacks {
};
export const load = (): ALGOStdlib => {

const connector = 'ALGO';
const reachStdlib = stdlib;
const { setQueryLowerBound, getQueryLowerBound, formatWithDecimals, handleFormat } = shared_impl;

const [ setSigningMonitor, notifySend ] = makeSigningMonitor<unknown, RecvTxn>();

// module-wide config
let customHttpEventHandler: (e: RHC.Event) => Promise<void> = async () => undefined;
function setCustomHttpEventHandler(h: (e: RHC.Event) => Promise<void>): void {
  customHttpEventHandler = h;
}
/**
 * @description client-side rate limiting.
 *  Setting this to any positive number will also prevent requests from being sent in parallel.
 *  Rate limiting is applied to all outgoing http requests, even if they are to different servers.
 */
let minMillisBetweenRequests: number = 0;
function setMinMillisBetweenRequests(n: number): void {
  minMillisBetweenRequests = n;
}
const reqLock = new Lock();
let currentReqNum: number | undefined = undefined;
let currentReqLabel: string | undefined = undefined;
let lastReqSentAt: number | undefined = undefined; // ms
const appStateMinRefreshMillis = 1000;

async function httpEventHandler(e: RHC.Event): Promise<void> {
  const en = e.eventName;
  if (minMillisBetweenRequests > 0) {
    if (en === 'before') {
      await reqLock.acquire();
      if (lastReqSentAt) {
        const waitMs = Math.max(0, lastReqSentAt + minMillisBetweenRequests - Date.now());
        debug(`waiting ${waitMs}ms due to minMillisBetweenRequests=${minMillisBetweenRequests}`);
        await Timeout.set(waitMs);
      }
      lastReqSentAt = Date.now();
      currentReqNum = e.reqNum;
      currentReqLabel = e.label;
    }
    if (en === 'success' || en === 'error') {
      if (currentReqNum === e.reqNum && currentReqLabel == e.label){
        currentReqNum = undefined;
        currentReqLabel = undefined;
        reqLock.release();
      } else {
        throw Error('impossible: multiple requests in flight');
      }
    }
  }
  await customHttpEventHandler(e);
}

// Helpers

// Parse CBR into Public Key
const cbr2algo_addr = (x:string): Address =>
  algosdk.encodeAddress(Buffer.from(x.slice(2), 'hex'));

// Takes a CBR Account or CBR Address and converts it into an Algod acceptable pubkey
const extractAddrConvert = (a: Account | Address): Address =>
  cbr2algo_addr(protect(T_Address, extractAddr(a)) as Address);

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

const indexerTxn2RecvTxn = (txn:IndexerTxn): RecvTxn => {
  const ait: IndexerAppTxn = txn['application-transaction'] || {};
  const aargs = ait['application-args'] || [];
  const aidx = ait['application-id'];
  // We're returning the first we find, but actually we just want the first one
  // period.
  const ccai = (() => {
    const its = txn['inner-txns']||[];
    for ( const itx of its ) {
      debug('ccai itx', itx);
      return itx['created-application-index'];
    }
    return undefined;
  })();
  return {
    'confirmed-round': txn['confirmed-round'],
    'sender': txn['sender'],
    'approval-program': ait['approval-program'],
    'clear-state-program': ait['clear-state-program'],
    'logs': (txn['logs'] || []),
    'application-args': aargs,
    'application-index': aidx,
    'created-application-index': txn['created-application-index'],
    'created-asset-index': txn['created-asset-index'],
    'created-companion-application-index': ccai,
  };
};

const waitForConfirmation = async (txId: TxId): Promise<RecvTxn> => {
  const doOrDie = async <X>(p: Promise<X>): Promise<OrExn<X>> => {
    try { return { val: await p }; }
    catch (e:any) { return { 'exn': e }; }
  };
  const dhead = `waitForConfirmation ${txId}`;
  const client = await getAlgodClient();

  const checkAlgod = async (): Promise<RecvTxn> => {
    const q = client.pendingTransactionInformation(txId).do() as unknown as Promise<AlgodTxn>;
    const infoM = await doOrDie(q);
    debug(dhead, 'info', infoM);
    if ( 'exn' in infoM ) {
      debug(dhead, 'switching to indexer on error');
      return await checkIndexer();
    }
    const info = infoM.val;
    const cr = info['confirmed-round'];
    if ( cr !== undefined && cr > 0 ) {
      const l = info['logs'] === undefined ? [] : info['logs'];
      debug(dhead, 'confirmed');
      const dtxn = algosdk.Transaction.from_obj_for_encoding(info['txn']['txn']);
      debug(dhead, 'confirmed', dtxn);
      const uToS = (a: Uint8Array[]|undefined) => (a || []).map((x: Uint8Array)=> uint8ArrayToStr(x, 'base64'));
      // We're returning the first we find, but actually we just want the first
      // one period.
      const ccai = (() => {
        const its = info['inner-txns']||[];
        for ( const itx of its ) {
          debug('ccai itx', itx);
          return itx['application-index'];
        }
        return undefined;
      })();
      return {
        'confirmed-round': cr,
        'created-asset-index': info['asset-index'],
        // @ts-ignore
        'logs': uToS(l),
        'created-application-index': info['application-index'],
        'created-companion-application-index': ccai,
        'sender': txnFromAddress(dtxn),
        'application-args': uToS(dtxn.appArgs),
      };
    } else if ( info['pool-error'] === '' ) {
      debug(dhead, 'still in pool, trying again');
      return await checkAlgod();
    } else {
      throw Error(`waitForConfirmation: error confirming: ${j2s(info)}`);
    }
  };

  const checkIndexer = async (): Promise<RecvTxn> => {
    const indexer = await getIndexer();
    const q = indexer.lookupTransactionByID(txId) as unknown as ApiCall<IndexerQuery1Res>;
    const res = await doQuery_(dhead, q);
    debug(dhead, 'indexer', res);
    return indexerTxn2RecvTxn(res['transaction']);
  };

  if ( await nodeCanRead() ) {
    return await checkAlgod();
  } else {
    return await checkIndexer();
  }
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
  let sapt_res: unknown;
  let notifyComplete: NotifyComplete<RecvTxn>;
  try {
    [ sapt_res, notifyComplete ] = await notifySend(txns, p.signAndPostTxns(txns));
  } catch (e:any) {
    const es = `${e}`;
    if ( hasProp(e, 'response') ) {
      const r = e.response;
      if ( hasProp(r, 'body') ) {
        e.response = r.body;
      } else if ( hasProp(r, 'text') ) {
        e.response = r.text;
      } else {
        delete r.request;
        delete r.req;
      }
    }
    throw { type: 'signAndPost', e, es };
  }
  debug({sapt_res});
  const N = txns.length - 1;
  const tN = decodeB64Txn(txns[N].txn);
  try {
    return await notifyComplete(waitForConfirmation(tN.txID())); // tN.lastRound
  } catch (e) {
    const es = `${e}`;
    throw { type: 'waitForConfirmation', e, es };
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
const stdWait = () => Timeout.set(1000);

const getTxnParams = async (label: string): Promise<TxnParams> => {
  const dhead = `${label} fillTxn`;
  debug(dhead, `getting params`);
  const client = await getAlgodClient();
  while (true) {
    const params_r = (await client.getTransactionParams().do()) as unknown as TxnParamsRaw;
    debug(dhead ,'got params:', params_r);
    const bi2n = (x:bigint): number => bigNumberToNumber(bigNumberify(x));
    const params: TxnParams = {
      ...params_r,
      fee: bi2n(params_r.fee),
      firstRound: bi2n(params_r.firstRound),
      lastRound: bi2n(params_r.lastRound),
    };
    debug(dhead ,'got params:', params);
    if (params.firstRound !== 0) {
      return params;
    }
    debug(dhead, `...but firstRound is 0, so let's wait and try again.`);
    await stdWait();
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
    throw Error(`${label} txn failed:\n${j2s(txn)}\nwith:\n${j2s(e)}`);
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
  const { unsupported, warnings } = algob;
  const render = (x: string[]) => x.map(s => ` * ${s}`).join('\n');
  if ( warnings.length > 0 && ! hideWarnings() ) {
    console.error(`This Reach application is dangerous to run on Algorand for the following reasons:\n${render(warnings)}`);
  }
  if ( unsupported.length > 0 ) {
    throw Error(`This Reach application is not supported on Algorand for the following reasons:\n${render(unsupported)}`);
  }
}

// Get these from stdlib
// const MaxTxnLife = 1000;
const MinTxnFee = 1000;
const MaxAppTxnAccounts = 4;
const MinBalance = 100000;
const MaxAppProgramLen = 2048

const SchemaMinBalancePerEntry = 25000
const SchemaBytesMinBalance = 25000
const SchemaUintMinBalance = 3500
const AppFlatParamsMinBalance = 100000
const AppFlatOptInMinBalance = 100000

const ui8h = (x:Uint8Array): string => Buffer.from(x).toString('hex');
const base64ToUI8A = (x:string): Uint8Array => Uint8Array.from(Buffer.from(x, 'base64'));
const base64ify = (x: WithImplicitCoercion<string>|Uint8Array): string => Buffer.from(x).toString('base64');

function looksLikeAccountingNotInitialized(e: any) {
  const responseText = e?.response?.text || null;
  // TODO: trust the response to be json and parse it?
  // const json = JSON.parse(responseText) || {};
  // const msg: string = (json.message || '').toLowerCase();
  const msg = (responseText || '').toLowerCase();
  return msg.includes(`accounting not initialized`);
}

const doQueryM_ = async <T>(dhead:string, query: ApiCall<T>): Promise<OrExn<T>> => {
  try {
    const val = await query.do();
    debug(dhead, 'RESULT', val);
    return { val };
  } catch (exn:any) {
    return { exn };
  }
};

const doQuery_ = async <T>(dhead:string, query: ApiCall<T>, howMany: number = 0, failOk:((e:any) => OrExn<T>) = ((exn:any) => { return { exn }; })): Promise<T> => {
  debug(dhead, query.query);
  while ( true ) {
    if ( howMany > 0 ) {
      await stdWait();
    }
    const res = await doQueryM_(dhead, query);
    if ( 'exn' in res ) {
      let e = res.exn;
      if ( e?.errno === -111 || e?.code === "ECONNRESET") {
        debug(dhead, 'NO CONNECTION');
      } else if ( looksLikeAccountingNotInitialized(e) ) {
        debug(dhead, 'ACCOUNTING NOT INITIALIZED');
      }
      if ( e?.response?.text ) { e = e.response.text; }
      const fr = failOk(e);
      if ( 'exn' in fr ) {
        debug(dhead, 'RETRYING', {e});
        howMany++;
      } else {
        debug(dhead, 'FAIL OK', {e, fr});
        return fr.val;
      }
    } else {
      return res.val;
    }
  }
};

function getValidQueryWindow(): number|true {
  return true;
}
function setValidQueryWindow(n: number|true): void {
  if (typeof n === 'number') {
    throw Error(`Only setValidQueryWindow(true) is supported on Algorand`);
  }
}

const isCreateTxn = (txn:IndexerTxn): boolean => {
  const at = txn['application-transaction'];
  return at ? bigNumberify(at['application-id']).eq(0) : false;
};
const emptyOptIn = (txn:IndexerTxn) => {
  const at = txn['application-transaction'];
  const ataa = at && at['application-args'] || [];
  return at ?
    (at['on-completion'] === 'optin' && ataa.length == 0)
    : false;
};
const apiOnly = (txn:IndexerTxn) => {
  const ls = txn['logs'];
  if ( ls && ls.length === 1 ) {
    const l0 = ls[0];
    const l0ui = base64ToUI8A(l0);
    if ( l0ui.length >= 4 ) {
      const l0h = ui8h(l0ui.subarray(0, 4));
      debug('apiOnly', { l0h });
      return (l0h === '151f7c75');
    } else {
      return false;
    }
  } else {
    return false;
  }
};
type EQInitArgs = {
  ApplicationID: BigNumber,
};
type EventQueue = IEventQueue<EQInitArgs, IndexerTxn, RecvTxn>;
const newEventQueue = (): EventQueue => {
  const getTxns = async (dhead:string, initArgs:EQInitArgs, ctime: BigNumber, howMany: number, limsug?: number): Promise<EQGetTxnsR<IndexerTxn>> => {
    const { ApplicationID } = initArgs;
    const indexer = await getIndexer();
    const mtime = bigNumberToNumber(ctime.add(1));
    debug(dhead, { ctime, mtime });
    const appn = bigNumberToNumber(ApplicationID);
    let query =
      indexer.searchForTransactions()
        .applicationID(appn)
        //.txType('appl')
        .minRound(mtime);
    // TODO: ignore limsug on randlabs?
    if (limsug) { query = query.limit(limsug); }
    const q = query as unknown as ApiCall<IndexerQueryMRes>
    const res = await doQuery_(dhead, q, howMany);
    const txns: Array<IndexerTxn> = [];
    const walkTxns = (ints:Array<IndexerTxn>) => {
      ints.filter((x:IndexerTxn) => x['tx-type'] === 'appl').forEach((x:IndexerTxn) => {
        const at = (x['application-transaction']||{});
        const ai = bigNumberify(at['application-id']||0);
        const cai = bigNumberify(x['created-application-index']||0);
        const its = x['inner-txns'];
        if (ai.eq(ApplicationID) || cai.eq(ApplicationID)) {
          txns.push(x);
        } else if (its) {
          walkTxns(its);
        }
      });
    };
    walkTxns(res.transactions);
    const gtime = bigNumberify(res['current-round']);
    return { txns, gtime };
  };
  const getTxnTime = (x:IndexerTxn): BigNumber => bigNumberify(x['confirmed-round']);
  return makeEventQueue<EQInitArgs, IndexerTxn, RecvTxn>({
    raw2proc: indexerTxn2RecvTxn,
    alwaysIgnored: (x) => (emptyOptIn(x) || apiOnly(x)),
    getTxns, getTxnTime,
  });
};

const { addressEq, assert, gt, protect } = stdlib;

const { T_UInt, T_Tuple, T_Contract, T_Bytes, T_Address, T_Digest } = typeDefs;

const { randomUInt, hasRandom } = makeRandom(8);

async function waitIndexerFromEnv(env: ProviderEnv): Promise<[BaseHTTPClient, algosdk.Indexer]> {
  const { ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT, ALGO_INDEXER_TOKEN, ALGO_INDEXER_TOKEN_HEADER } = env;
  await waitPort(ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);
  const port = ALGO_INDEXER_PORT || undefined; // UTBC checks for undefined
  const utbc = new UTBC.URLTokenBaseHTTPClient({[ALGO_INDEXER_TOKEN_HEADER]: ALGO_INDEXER_TOKEN}, ALGO_INDEXER_SERVER, port);
  const rhc = new RHC.ReachHTTPClient(utbc, 'indexer', httpEventHandler);
  return [rhc, new algosdk.Indexer(rhc)];
}

async function waitAlgodClientFromEnv(env: ProviderEnv): Promise<[BaseHTTPClient, algosdk.Algodv2]> {
  const { ALGO_SERVER, ALGO_PORT, ALGO_TOKEN, ALGO_TOKEN_HEADER } = env;
  await waitPort(ALGO_SERVER, ALGO_PORT);
  const port = ALGO_PORT || undefined;  // UTBC checks for undefiend
  const utbc = new UTBC.URLTokenBaseHTTPClient({[ALGO_TOKEN_HEADER]: ALGO_TOKEN}, ALGO_SERVER, port);
  const rhc = new RHC.ReachHTTPClient(utbc, 'algodv2', httpEventHandler);
  return [rhc, new algosdk.Algodv2(rhc)];
}

const makeProviderByWallet = async (wallet:ARC11_Wallet, env: any): Promise<Provider> => {
  debug(`making provider with wallet`);
  const defaults = { REACH_ISOLATED_NETWORK: 'no', ALGO_NODE_WRITE_ONLY: 'yes' }; // pessimistic
  const allEnv = { ...defaults, ...env, ...(wallet._env || {}) }; // rightmost is preferred
  const { ALGO_GENESIS_ID, ALGO_GENESIS_HASH, ALGO_ACCOUNT } = env;
  const { REACH_ISOLATED_NETWORK, ALGO_NODE_WRITE_ONLY } = allEnv;
  const walletOpts: EnableOpts = {
    genesisID: ALGO_GENESIS_ID || undefined,
    genesisHash: ALGO_GENESIS_HASH || undefined,
    accounts: ALGO_ACCOUNT ? [ ALGO_ACCOUNT ] : undefined,
  };
  const isIsolatedNetwork = truthyEnv(REACH_ISOLATED_NETWORK);
  const nodeWriteOnly = truthyEnv(ALGO_NODE_WRITE_ONLY);
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
  const algod_bc = await wallet.getAlgodv2Client();
  const indexer_bc = await wallet.getIndexerClient();
  const algodClient = new algosdk.Algodv2(algod_bc);
  const indexer = new algosdk.Indexer(indexer_bc);
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
  return { algod_bc, indexer_bc, indexer, algodClient, nodeWriteOnly, getDefaultAddress, isIsolatedNetwork, signAndPostTxns };
};

const setWalletFallback = (wf:() => unknown) => {
  if ( ! window.algorand ) { window.algorand = wf(); }
};

const checkNetwork = (ret: EnableNetworkResult, eopts?: EnableOpts): void => {
  const { genesisID:  id, genesisHash:  h } = ret;
  const { genesisID: eid, genesisHash: eh } = eopts || {};
  if ( ( eid && eid !== id) || ( eh && eh !== h ) ) {
    throw Error(
      `Requested genesis ID or hash not supported by this wallet.\n`
      + `Expected: '${id}' '${h}'\n`
      + `Got: '${eid}' '${eh}'`
    );
  }
}

const checkAccounts = (addr: string, got?: string[]): void => {
  if ( got && ( got[0] !== addr || got.length > 1 ) ) {
    throw Error(
      `One or more requested accounts not supported by this wallet.\n`
      + `Expected: ${JSON.stringify([addr])}\n`
      + `Got: ${JSON.stringify(got)}`
    );
  }
}

const doWalletFallback_signOnly = (opts:any, getAddr:() => Promise<string>, signTxns_:(txns:string[]) => Promise<string[]>, disconnect_:() => Promise<void>): ARC11_Wallet => {
  let p: BasicProvider|undefined = undefined;
  const base = opts['providerEnv'] || 'LocalHost';
  const _env = typeof base === 'string' ? providerEnvByName(base) : base;
  const enableNetwork = async (eopts?: EnableOpts): Promise<EnableNetworkResult> => {
    const op = opts.provider;
    if ( op ) { p = (op as BasicProvider); }
    else { p = await makeProviderByEnv(_env); }
    const { genesisID, genesisHash } = await p.algodClient.getTransactionParams().do();
    const ret = { genesisID, genesisHash };
    checkNetwork(ret, eopts);
    return ret;
  };
  const enableAccounts = async (eopts?: EnableAccountsOpts): Promise<EnableAccountsResult> => {
    const addr = await getAddr();
    checkAccounts(addr, eopts?.accounts);
    return { accounts: [ addr ] };
  };
  const enable = async (eopts?:object): Promise<EnableResult> => {
    const nres = await enableNetwork(eopts);
    const ares = await enableAccounts(eopts);
    return { ...nres, ...ares };
  };
  const getAlgodv2Client = async () => {
    if ( !p ) { throw new Error(`must call enable`) };
    return p.algod_bc;
  }
  const getIndexerClient = async () => {
    if ( !p ) { throw new Error(`must call enable`) };
    return p.indexer_bc;
  }
  const signTxns = async (txns:WalletTransaction[], sopts?:object) => {
    void(sopts);
    debug(`fallBack: signAndPostTxns`, {txns});
    const to_sign: string[] = [];
    txns.forEach((txn) => {
      if ( ! txn.stxn ) {
        to_sign.push(txn.txn);
      }
    });
    debug(`fallBack: signAndPostTxns`, {to_sign});
    const signed: string[] = to_sign.length == 0 ? [] : await signTxns_(to_sign);
    debug(`fallBack: signAndPostTxns`, {signed});
    const stxns: string[] = txns.map((txn) => {
      if ( txn.stxn ) { return txn.stxn; }
      const s = signed.shift();
      if ( ! s ) { throw new Error(`txn not signed`); }
      return s;
    });
    return stxns;
  };
  const postTxns = async (stxns: string[], popts?:object) => {
    if ( !p ) { throw new Error(`must call enable`) };
    void(popts);
    const bs = stxns.map((stxn) => Buffer.from(stxn, 'base64'));
    debug(`fallBack: signAndPostTxns`, bs);
    await p.algodClient.sendRawTransaction(bs).do();
    return {}; // TODO
  }
  const signAndPostTxns = async (txns:WalletTransaction[], spopts?:object) => {
    if ( !p ) { throw new Error(`must call enable`) };
    const stxns = await signTxns(txns, spopts);
    return await postTxns(stxns, spopts);
  };
  const disconnect = async () => {
    return await disconnect_();
  };
  return { _env, enable, enableNetwork, enableAccounts, getAlgodv2Client, getIndexerClient, signTxns, postTxns, signAndPostTxns, disconnect };
};
const walletFallback_mnemonic = (opts:object) => (): ARC11_Wallet => {
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
  const disconnect = async () => {
  };
  return doWalletFallback_signOnly(opts, getAddr, signTxns, disconnect);
};
const walletFallback_MyAlgoWallet = (MyAlgoConnect: unknown, opts: object) => (): ARC11_Wallet => {
  debug(`using MyAlgoWallet wallet fallback`);
  // Workaround for known webpack issue w/ MAW 1.1.2 & earlier
  // https://github.com/randlabs/myalgo-connect/issues/27
  if (!window.Buffer) window.Buffer = Buffer;
  // @ts-ignore
  const mac: MyAlgoConnect = new MyAlgoConnect();
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
  const disconnect = async () => {
  };
  return doWalletFallback_signOnly(opts, getAddr, signTxns, disconnect);
};
interface ARC11_WalletWC extends ARC11_Wallet { wc: any };
const walletFallback_WalletConnect = (WalletConnect:any, opts:any) => (): ARC11_WalletWC => {
  debug(`using WalletConnect wallet fallback`);
  const wc = (opts.WalletConnect_wc as any) || new WalletConnect();
  const disconnect = (() => wc.disconnect());
  const wallet = doWalletFallback_signOnly(opts, (() => wc.getAddr()), ((ts) => wc.signTxns(ts)), disconnect);
  return { ...wallet, wc };
};
const walletFallback = (opts:any) => {
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

const [getProvider, setProvider] = replaceableThunk(async () => {
  if ( window.algorand ) {
    // @ts-ignore
    return await makeProviderByWallet(window.algorand, process.env);
  } else {
    debug(`making default provider based on process.env`);
    return await makeProviderByEnv(process.env);
  }
});
const getAlgodClient = async () => {
  const c = (await getProvider()).algodClient;
  c.setIntEncoding(algosdk.IntDecoding.BIGINT);
  return c;
};
const getIndexer = async () => {
  const p = (await getProvider()).indexer;
  p.setIntEncoding(algosdk.IntDecoding.BIGINT);
  return p;
};
const nodeCanRead = async () => ((await getProvider()).nodeWriteOnly === false);
const ensureNodeCanRead = async () =>
  assert(await nodeCanRead(), "node can read" );

const localhostProviderEnv: ProviderEnv = {
  ALGO_SERVER: 'http://localhost',
  ALGO_PORT: '4180',
  ALGO_TOKEN_HEADER: defaultALGO_TOKEN_HEADER,
  ALGO_TOKEN: rawDefaultToken,
  ALGO_INDEXER_SERVER: 'http://localhost',
  ALGO_INDEXER_PORT: '8980',
  ALGO_INDEXER_TOKEN_HEADER: defaultALGO_INDEXER_TOKEN_HEADER,
  ALGO_INDEXER_TOKEN: rawDefaultItoken,
  REACH_ISOLATED_NETWORK: 'yes',
  ALGO_NODE_WRITE_ONLY: 'no',
}

function envDefaultsALGO(env: Partial<ProviderEnv>): ProviderEnv {
  const denv = localhostProviderEnv;
  // @ts-ignore
  const ret: ProviderEnv = {};
  for ( const f of ['ALGO_SERVER', 'ALGO_PORT', 'ALGO_TOKEN_HEADER', 'ALGO_TOKEN', 'ALGO_INDEXER_SERVER', 'ALGO_INDEXER_PORT', 'ALGO_INDEXER_TOKEN_HEADER', 'ALGO_INDEXER_TOKEN', 'REACH_ISOLATED_NETWORK', 'ALGO_NODE_WRITE_ONLY'] ) {
    // @ts-ignore
    ret[f] = envDefault(env[f], denv[f]);
  }
  return ret;
};

async function makeProviderByEnv(env: Partial<ProviderEnv>): Promise<Provider> {
  debug(`makeProviderByEnv`, env);
  const fullEnv = envDefaultsALGO(env);
  debug(`makeProviderByEnv defaulted`, fullEnv);
  const [algod_bc, algodClient] = await waitAlgodClientFromEnv(fullEnv);
  const [indexer_bc, indexer] = await waitIndexerFromEnv(fullEnv);
  const isIsolatedNetwork = truthyEnv(fullEnv.REACH_ISOLATED_NETWORK);
  const nodeWriteOnly = truthyEnv(fullEnv.ALGO_NODE_WRITE_ONLY);
  const errmsg = (s: string) =>
    `Providers created by environment ${s}. Calling setProviderByEnv or setProviderByName removes this capability. Try removing calls to those functions.`
  const getDefaultAddress = async (): Promise<Address> => {
    throw new Error(errmsg(`do not have default addresses`));
  };
  const signAndPostTxns = async (txns:WalletTransaction[], opts?:object) => {
    void(opts);
    const stxns = txns.map((txn) => {
      if ( txn.stxn ) { return txn.stxn; }
      throw new Error(errmsg(`cannot interactively sign`));
    });
    const bs = stxns.map((stxn) => Buffer.from(stxn, 'base64'));
    debug(`signAndPostTxns`, bs);
    await algodClient.sendRawTransaction(bs).do();
  };
  return { algod_bc, indexer_bc, algodClient, indexer, nodeWriteOnly, isIsolatedNetwork, getDefaultAddress, signAndPostTxns };
};
function setProviderByEnv(env: Partial<ProviderEnv>): void {
  setProvider(makeProviderByEnv(env));
};

function algonodeEnv(net: string): ProviderEnv {
  // works for MainNet, TestNet, and BetaNet
  // https://algonode.io/api/#node-api
  const prefix = `https://${net.toLowerCase()}-`;
  const suffix = `.algonode.cloud`;
  return {
    ALGO_SERVER: `${prefix}api${suffix}`,
    ALGO_PORT: ``,
    ALGO_TOKEN_HEADER: defaultALGO_TOKEN_HEADER,
    ALGO_TOKEN: ``,
    ALGO_INDEXER_SERVER: `${prefix}idx${suffix}`,
    ALGO_INDEXER_PORT: ``,
    ALGO_INDEXER_TOKEN_HEADER: defaultALGO_INDEXER_TOKEN_HEADER,
    ALGO_INDEXER_TOKEN: ``,
    REACH_ISOLATED_NETWORK: 'no',
    ALGO_NODE_WRITE_ONLY: 'yes', // XXX no?
  }
}

function randlabsProviderEnv(net: string): ProviderEnv {
  const prefix = net === 'MainNet' ? '' : `${net.toLowerCase()}.`;
  const RANDLABS_BASE = `${prefix}algoexplorerapi.io`;
  return {
    ALGO_SERVER: `https://node.${RANDLABS_BASE}`,
    ALGO_PORT: '',
    ALGO_TOKEN_HEADER: defaultALGO_TOKEN_HEADER,
    ALGO_TOKEN: '',
    // TODO: update to just indexer.
    ALGO_INDEXER_SERVER: `https://algoindexer.${RANDLABS_BASE}`,
    ALGO_INDEXER_PORT: '',
    ALGO_INDEXER_TOKEN_HEADER: defaultALGO_INDEXER_TOKEN_HEADER,
    ALGO_INDEXER_TOKEN: '',
    REACH_ISOLATED_NETWORK: 'no',
    ALGO_NODE_WRITE_ONLY: 'yes',
  }
}

function providerEnvByName(pn: ProviderName): ProviderEnv {
  switch (pn) {
    case 'MainNet': return algonodeEnv('MainNet');
    case 'TestNet': return algonodeEnv('TestNet');
    case 'BetaNet': return algonodeEnv('BetaNet');
    case 'randlabs/MainNet': return randlabsProviderEnv('MainNet');
    case 'randlabs/TestNet': return randlabsProviderEnv('TestNet');
    case 'randlabs/BetaNet': return randlabsProviderEnv('BetaNet');
    case 'algonode/MainNet': return algonodeEnv('MainNet');
    case 'algonode/TestNet': return algonodeEnv('TestNet');
    case 'algonode/BetaNet': return algonodeEnv('BetaNet');
    case 'LocalHost': return localhostProviderEnv;
    default: throw Error(`Unrecognized provider name: ${pn}`);
  }
}

function setProviderByName(pn: ProviderName): void {
  return setProviderByEnv(providerEnvByName(pn));
}

// eslint-disable-next-line max-len
const rawFaucetDefaultMnemonic = 'guilt butter canyon devote inflict comfort lumber relief chat key fury absorb reject palm siege draw jelly lyrics melody palace use box joy ability result';
const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  const FAUCET = algosdk.mnemonicToSecretKey(
    envDefault(process.env.ALGO_FAUCET_PASSPHRASE, rawFaucetDefaultMnemonic),
  );
  return await connectAccount(FAUCET);
});

const str2note = (x:string) => new Uint8Array(Buffer.from(x));
const NOTE_Reach_str = `Reach ${VERSION}`;
const NOTE_Reach = str2note(NOTE_Reach_str);
const NOTE_Reach_tag = (tag?:number) => tag ? str2note(NOTE_Reach_str + ` ${tag})`) : NOTE_Reach;

const makeTransferTxn = (
  from: Address,
  to: Address,
  value: BigNumber,
  token: Token|undefined,
  ps: TxnParams,
  closeTo?: Address,
  tag?: number,
  note?: Uint8Array,
): Transaction => {
  const valuen = bigNumberToBigInt(value);
  const note_ = note ?? NOTE_Reach_tag(tag);
  const txn =
    token ?
      algosdk.makeAssetTransferTxnWithSuggestedParams(
        from, to, closeTo, undefined,
        valuen, note_, bigNumberToNumber(token), ps)
    :
      algosdk.makePaymentTxnWithSuggestedParams(
        from, to, valuen, closeTo, note_, ps);
  return txn;
};

const transfer = async (
  from: Account,
  to: Account | Address,
  value: any,
  token?: Token,
  opts?: TransferOpts,
): Promise<RecvTxn> => {
  const sender = from.networkAccount;
  const receiver = extractAddrConvert(to);
  const valuebn = bigNumberify(value);
  const ps = await getTxnParams('transfer');
  const closeTo = opts?.closeTo ? cbr2algo_addr(protect(T_Address, opts.closeTo) as Address) : undefined;
  const txn = toWTxn(makeTransferTxn(sender.addr, receiver, valuebn, token,
                                     ps, closeTo, opts?.tag, opts?.note));

  return await sign_and_send_sync(
    `transfer ${j2s(from)} ${j2s(to)} ${valuebn}`,
    sender,
    txn);
};

interface LogRep {
  parse: (log: string) => (any[]|undefined),
  parse0: (txn: RecvTxn) => (any[]|undefined),
  parse0b: (txn: RecvTxn) => boolean,
};
const makeLogRep = (evt:string, tys:AnyALGO_Ty[]): LogRep => {
  const hLen = 4;
  const tyns = tys.map(ty => ty.netName);
  const sig = `${evt}(${tyns.join(',')})`;
  const hu = sha512_256(sig);
  const hp = hu.slice(0, hLen*2); // hu is hex nibbles
  const trunc = (x: string): string => ui8h(base64ToUI8A(x).slice(0, hLen));
  debug(`makeLogRep`, { evt, tyns, sig, hu, hp });
  const parse = (log:string): (any[]|undefined) => {
    if ( trunc(log) !== hp ) { return undefined; }
    debug(`parse`, { log });
    // @ts-ignore
    const [ logb, ...pd ] = T_Tuple([bytestringyNet(hLen), ...tys]).fromNet(reNetify(log));
    debug(`parse`, { logb, pd });
    return pd;
  };
  const parse0 = (txn:RecvTxn): (any[]|undefined) => {
    if ( txn.logs.length == 0 ) { return undefined; }
    const log = txn.logs[0];
    return parse(log);
  };
  const parse0b = (txn:RecvTxn) => parse0(txn) !== undefined;
  return { parse, parse0, parse0b };
};

const reachEvent = (i:number) => `_reach_e${i}`;
const makeHasLogFor = (i:number, tys:AnyALGO_Ty[]) => {
  debug(`hasLogFor`, i, tys);
  const lr = makeLogRep(reachEvent(i), tys);
  return lr.parse0b;
};

/** @description base64->hex->arrayify */
const reNetify = (x: string): NV => {
  const s: string = Buffer.from(x, 'base64').toString('hex');
  return ethers.utils.arrayify('0x' + s);
};

const getAccountInfo  = async (acc: Account | Address): Promise<AccountInfo> => {
  // Note restrictions on AccountInfo responses due to 'MaxAPIResourcesPerAccount' setting in the node and indexer
  // https://developer.algorand.org/articles/algorand-unlimited-assets-and-smart-contracts/
  const addr = extractAddrConvert(acc);
  const dhead = 'getAddressInfo';
  try {
    await ensureNodeCanRead();
    const client = await getAlgodClient();
    const req = client.accountInformation(addr);
    debug(dhead, req);
    const res = (await req.do()) as AccountInfo;
    debug(dhead, 'node', res);
    return res;
  } catch (e:any) {
    debug(dhead, 'node err', e);
  }
  const indexer = await getIndexer();
  const failOk = (x:any): OrExn<IndexerAccountInfoRes> => {
    if ( typeof x === 'string' && x.includes('no accounts found for address') ) {
      return { val: {
        'current-round': BigInt(0),
        'account': {
          'amount': BigInt(0),
        }
      } };
    } else {
      return { exn: x };
    }
  };
  const query = indexer.lookupAccountByID(addr) as unknown as ApiCall<IndexerAccountInfoRes>;
  const res = await doQuery_(dhead, query, 0, failOk);
  debug(dhead, res);
  return res.account;
};

const getAssetInfo = async (a:number): Promise<AssetInfo> => {
  const dhead = 'getAssetInfo';
  const indexer = await getIndexer();
  const q = indexer.lookupAssetByID(a) as unknown as ApiCall<IndexerAssetInfoRes>;
  const failOk = (x:any): OrExn<IndexerAssetInfoRes> => {
    if (typeof x === 'string' && x.includes('no assets found for asset-id') ) {
      throw Error(`Asset ${a} does not exist`);
    } else {
      return { exn: x };
    }
  };
  const res = await doQuery_<IndexerAssetInfoRes>(dhead, q, 0, failOk);
  debug(dhead, res);
  return res.asset;
};

const getApplicationInfoM = async (idn:BigNumber): Promise<OrExn<AppInfo>> => {
  const id = bigNumberToNumber(idn);
  const dhead = 'getApplicationInfo';

  // First, lookup application in algod
  try {
    await ensureNodeCanRead();
    const client = await getAlgodClient();
    const res = (await client.getApplicationByID(id).do()) as AppInfo;
    debug(dhead, 'node', res);
    return { val: res };
  } catch (e:any) {
    debug(dhead, 'node err', e);
  }

  // If algod couldn't find it, lookup application in indexer
  const indexer = await getIndexer();
  const query = indexer.lookupApplications(id)
                       .includeAll(true) as unknown as ApiCall<IndexerAppInfoRes>;
  const queryRes = await doQueryM_(dhead, query);

  if ('val' in queryRes) {
    debug(dhead, {application: queryRes.val.application});
    // If application was deleted, synthesize AppInfo from transaction data
    return queryRes.val.application.deleted ? getDeletedApplicationInfoM(id) : { val: queryRes.val.application };
  } else {
    return queryRes;
  }
};

const getDeletedApplicationInfoM = async (id: number): Promise<OrExn<AppInfo>> => {
  const dhead = 'getDeletedApplicationInfoM'
  const indexer = await getIndexer();
  const query = indexer.searchForTransactions()
                       // .txType('appl')
                       .applicationID(id)
                       .limit(1) as unknown as ApiCall<IndexerQueryMRes>;
  const queryRes = await doQueryM_(dhead, query);

  if ('val' in queryRes) {
    if (queryRes.val.transactions.length === 0) {
      return { exn: 'application does not exist' };
    }

    const txn = queryRes.val.transactions[0];
    const appTxn = txn['application-transaction'];
    debug(dhead, {appTxn});

    if (txn['tx-type'] !== 'appl'
        || appTxn === undefined
        || txn['created-application-index'] !== BigInt(id)
        || appTxn['application-id'] !== BigInt(0)
        || appTxn['approval-program'] === undefined
        || appTxn['clear-state-program'] === undefined
        || appTxn['local-state-schema'] === undefined
        || appTxn['global-state-schema'] === undefined)
    {
      return { exn: 'tried to synthesize AppInfo from deployment transaction, but the deployment transaction was wrong' };
    }

    return { val: {
      'id': txn['created-application-index'],
      'created-at-round': txn['confirmed-round'],
      'deleted': true,
      'params': {
        'creator': txn['sender'],
        'approval-program': appTxn['approval-program'],
        'clear-state-program': appTxn['clear-state-program'],
        'local-state-schema': appTxn['local-state-schema'],
        'global-state-schema': appTxn['global-state-schema'],
        'global-state': [],
        'extra-program-pages': appTxn['extra-program-pages'] ?? BigInt(0),
      },
    }}
  } else {
    return queryRes;
  }
}


const getLocalState_ = async (addr: Address, ApplicationID: BigNumber): Promise<AppStateKVs | undefined> => {
  const dhead = 'getLocalState';
  if (await nodeCanRead()) {
    const client = await getAlgodClient();
    const query = client.accountApplicationInformation(addr, bigNumberToNumber(ApplicationID)) as unknown as ApiCall<AccountApplicationInfo>;
    const accAppInfo = await doQuery_('contract.getLocalState', query, 0, _ => { return { val: undefined }; });
    return accAppInfo?.['app-local-state']?.['key-value'];
  } else {
    const indexer = await getIndexer();
    const query = indexer
      .lookupAccountAppLocalStates(addr)
      .applicationID(bigNumberToNumber(ApplicationID)) as unknown as ApiCall<IndexerAccountAppLocalStatesRes>;
    const appLocalStatesRes = await doQuery_(dhead, query);
    // As of indexer version 2.11.1 (at least, possibly higher versions too),
    // apps-local-states can come back 'null', equivalent to an empty array
    const appsLocalStates = appLocalStatesRes['apps-local-states'] ?? [];
    const appLocalState = appsLocalStates.find(app => ApplicationID.eq(app['id']));
    return appLocalState?.['key-value'];
  }
};

const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const thisAcc = networkAccount;
  let label = thisAcc.addr.substring(2, 6);
  const pks = T_Address.canonicalize(thisAcc);
  debug(label, 'connectAccount');
  let createTag = 0;

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

    const { stateSize, stateKeys, mapDataKeys, mapDataSize, ABI, companionInfo } = bin._Connectors.ALGO;
    const hasCompanion = companionInfo !== null;
    const hasMaps = mapDataKeys > 0;
    const { mapDataTy } = bin._getMaps({reachStdlib: stdlib});
    const emptyMapDataTy = T_Bytes(mapDataTy.netSize);
    const emptyMapData =
      // This is a bunch of Nones
      mapDataTy.fromNet(
        emptyMapDataTy.toNet(emptyMapDataTy.canonicalize('')));
    debug({ emptyMapData });

    type GlobalState = [BigNumber, BigNumber, ContractInfo];
    type ContractHandler = {
      ApplicationID: BigNumber,
      Deployer: Address,
      viewMapRef: (mapi:number, a:Address) => Promise<any>,
      ensureOptIn: (() => Promise<void>),
      getAppState: (() => Promise<AppStateKVs|undefined>),
      getGlobalState: ((appSt_g?:AppStateKVs|undefined) => Promise<GlobalState|undefined>),
      canIWin: ((lct:BigNumber) => Promise<boolean>),
      isIsolatedNetwork: (() => boolean),
      ctcAddr: Address,
    };
    type GetC = () => Promise<ContractHandler>;

    const makeGetC = (setupViewArgs: SetupViewArgs, eq: EventQueue) => {
      const { getInfo } = setupViewArgs;
      let _theC: ContractHandler|undefined = undefined;
      return async (): Promise<ContractHandler> => {
        debug(label, 'getC');
        if ( _theC ) {
          debug(label, 'getC', 'ret');
          return _theC;
        }
        debug(label, 'getC', 'wait');
        const ctcInfo = await getInfo();
        const { ApplicationID, Deployer } =
          await stdVerifyContract( setupViewArgs, (async () => {
            return await verifyContract_(label, ctcInfo, bin, eq);
          }));
        if ( ! eq.isInited() ) {
          eq.init({ ApplicationID });
          eq.pushIgnore(isCreateTxn);
        }
        debug(label, 'getC', {ApplicationID} );

        const ctcAddr = algosdk.getApplicationAddress(bigNumberToBigInt(ApplicationID));
        debug(label, 'getC', { ctcAddr });

        // Read map data
        const getLocalState = async (addr: Address): Promise<AppStateKVs | undefined> => {
          return await getLocalState_(addr, ApplicationID);
        }

        // Application Local State Opt-in
        const didOptIn = async (): Promise<boolean> =>
          (await doAccountAppOptedIn(thisAcc.addr, ApplicationID));
        const doOptIn = async (): Promise<void> =>
          (await doAccountAppOptIn(thisAcc, ApplicationID));

        let ensuredOptIn: boolean = false;
        const ensureOptIn = async (): Promise<void> => {
          if ( ! ensuredOptIn ) {
            if ( ! await didOptIn() ) {
              await doOptIn();
            }
            ensuredOptIn = true;
          }
        };

        let lastAppState : AppStateKVs|undefined = undefined;
        let lastAppStateTime : number = 0;
        const getAppState = async () => {
          const now = Date.now();
          const minMillis = isIsolatedNetwork() ? 0 : appStateMinRefreshMillis;
          if ( lastAppState && now - lastAppStateTime < minMillis){
            debug('getAppState cached');
            return lastAppState;
          }
          lastAppState = await getAppStateFresh();
          lastAppStateTime = now;
          return lastAppState;
        };
        const getAppStateFresh = async (): Promise<AppStateKVs|undefined> => {
          const lab = `getAppStateFresh`;
          const appInfoM = await getApplicationInfoM(ApplicationID);
          if ( 'exn' in appInfoM || appInfoM.val.deleted ) {
            return undefined;
          }
          const appInfo = appInfoM.val;
          const appSt = appInfo['params']['global-state'];
          debug(lab, {appSt});
          return appSt;
        };
        const getGlobalState = async (appSt_g?:AppStateKVs|undefined): Promise<GlobalState|undefined> => {
          const appSt = appSt_g || await getAppState();
          if ( !appSt ) { return undefined; }
          const gsbs = readStateBytes('', [], appSt);
          if ( !gsbs ) { return undefined; }
          // `map gvType keyState_gvs` in Haskell
          const mCompanion = hasCompanion ? [ T_Contract ] : [];
          const gty = T_Tuple([T_UInt, T_UInt, ...mCompanion]);
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

        const viewMapRef = async (mapi: number, a:Address): Promise<any> => {
          debug('viewMapRef', { mapi, a });
          const ls = await getLocalState(cbr2algo_addr(a));
          if ( ls === undefined ) { return ['None', null]; }
          debug('viewMapRef', { ls });
          const mbs = recoverSplitBytes('m', mapDataSize, mapDataKeys, ls);
          debug('viewMapRef', { mbs });
          if ( mbs === undefined ) { return ['None', null]; }
          const md = mapDataTy.fromNet(mbs);
          debug('viewMapRef', { md });
          // @ts-ignore
          const mr = md[mapi];
          assert(mr !== undefined, 'viewMapRef mr undefined');
          return mr;
        };

        return (_theC = { ApplicationID, ctcAddr, Deployer, getAppState, getGlobalState, ensureOptIn, canIWin, isIsolatedNetwork, viewMapRef });
      };
    };

    const getCurrentStep_ = async (ch:ContractHandler): Promise<BigNumber> => {
      const { getAppState, getGlobalState } = ch;
      const appSt = await getAppState();
      if ( !appSt ) { throw Error(`getCurrentStep_: no appSt`); }
      const gs = await getGlobalState(appSt);
      if ( !gs ) { throw Error(`getCurrentStep_: no gs`); }
      return gs[0];
    }

    const getState_ = async (getC:GetC, lookup:((vibna:BigNumber) => AnyALGO_Ty[])): Promise<[ContractInfo|undefined, Array<any>]> => {
      const { getAppState, getGlobalState } = await getC();
      const appSt = await getAppState();
      if ( !appSt ) { throw Error(`getState: no appSt`); }
      const gs = await getGlobalState(appSt);
      if ( !gs ) { throw Error(`getState: no gs`); }
      debug('getState_', {gs});
      const vvn = recoverSplitBytes('v', stateSize, stateKeys, appSt);
      if ( vvn === undefined ) { throw Error(`getState: no vvn`); }
      const vi = gs[0];
      const vtys = lookup(vi);
      const vty = T_Tuple(vtys);
      const vvs = vty.fromNet(vvn);
      debug(`getState_`, { vvn, vvs });
      return [ gs[2], vvs ];
    };

    const _setup = (setupArgs: SetupArgs): SetupRes => {
      const { setInfo, setTrustedVerifyResult } = setupArgs;

      const eq = newEventQueue();
      const getC = makeGetC(setupArgs, eq);
      let companionApp: ContractInfo|undefined = undefined;

      // Returns address of a Reach contract
      const getContractAddress = async () => {
        const { ctcAddr } = await getC();
        return T_Address.canonicalize(ctcAddr);
      };
      const getContractInfo = async () => {
        const { ApplicationID } = await getC();
        return ApplicationID;
      };
      const getContractCompanion = async (): Promise<MaybeRep<ContractInfo>> => {
        if ( hasCompanion ) {
          return ['None', null];
        } else {
          // @ts-ignore
          return ['Some', companionApp];
        }
      };

      const getCurrentStep = async () => {
        return await getCurrentStep_(await getC());
      }

      const getState = async (vibne:BigNumber, vtys:AnyALGO_Ty[]): Promise<Array<any>> => {
        debug('getState');
        const [ ci, ans ] = await getState_(getC, (vibna:BigNumber) => {
          if ( vibne.eq(vibna) ) { return vtys; }
          throw apiStateMismatchError(bin, vibne, vibna);
        });
        companionApp = ci;
        return ans;
      };

      const apiMapRef = (i:number, ty:AnyALGO_Ty): MapRefT<any> => async (f:string): Promise<MaybeRep<any>> => {
        void(ty);
        const { viewMapRef } = await getC();
        return await viewMapRef(i, f);
      };

      const simTokenAccepted = async (sim_r:any, addr:any, tok:any): Promise<boolean> => {
        simTokenAccepted_(sim_r, addr, tok);
        return (await balanceOfMNonNet(addr, tok)) !== false;
      };

      const sendrecv = async (srargs:SendRecvArgs): Promise<Recv> => {
        const { funcNum, evt_cnt, lct, tys, args, pay, out_tys, onlyIf, soloSend, timeoutAt, sim_p } = srargs;
        const isCtor = (funcNum === 0);
        const doRecv = async (didSend: boolean, waitIfNotPresent: boolean, msg: string): Promise<Recv> => {
          debug(dhead, `doRecv`, msg);
          if ( ! didSend && lct.eq(0) ) {
            throw new Error(`API call failed: ${msg}`);
          }
          return await recv({funcNum, evt_cnt, out_tys, didSend, waitIfNotPresent, timeoutAt});
        };
        const funcName = `m${funcNum}`;
        const dhead = `${label}: sendrecv ${funcName} ${timeoutAt}`;
        if ( ! onlyIf ) {
          return await doRecv(false, true, `onlyIf false`);
        }

        const trustedRecv = async (txn:RecvTxn): Promise<Recv> => {
          const didSend = true;
          const correctStep = makeHasLogFor(funcNum, out_tys);
          eq.pushIgnore((x:IndexerTxn) => correctStep(indexerTxn2RecvTxn(x)));
          return await recvFrom({dhead, out_tys, didSend, funcNum, txn});
        };

        if ( isCtor ) {
          debug(dhead, 'deploy');
          must_be_supported(bin);
          const { appApproval, appClear, extraPages } = bin._Connectors.ALGO;
          debug(dhead, `deploy`, {extraPages});
          const Deployer = thisAcc.addr;
          const createRes =
            await sign_and_send_sync(
              'ApplicationCreate',
              thisAcc,
              toWTxn(algosdk.makeApplicationCreateTxn(
                Deployer, await getTxnParams(dhead),
                algosdk.OnApplicationComplete.NoOpOC,
                base64ToUI8A(appApproval),
                base64ToUI8A(appClear),
                appLocalStateNumUInt, appLocalStateNumBytes + mapDataKeys,
                appGlobalStateNumUInt, appGlobalStateNumBytes + stateKeys,
                undefined, undefined, undefined, undefined,
                NOTE_Reach_tag(createTag++), undefined, undefined, extraPages)));

          const ai = createRes['created-application-index'];
          if ( ! ai ) {
            throw Error(`No created-application-index in ${j2s(createRes)}`);
          }
          const ApplicationID = bigNumberify(ai);
          debug(label, `created`, {ApplicationID});
          const ctcInfo = ApplicationID;
          setTrustedVerifyResult({ ApplicationID, Deployer });
          setInfo(ctcInfo);
        }
        const { ApplicationID, ctcAddr, Deployer, ensureOptIn, canIWin, isIsolatedNetwork } = await getC();

        const [ value, toks ] = pay;
        void(toks); // <-- rely on simulation because of ordering

        debug(dhead, '--- START');

        const curTime = await getNetworkTime();
        const curSecs = await getTimeSecs(curTime);

        const [ _svs, msg ] = argsSplit(args, evt_cnt);
        const [ _svs_tys, msg_tys ] = argsSplit(tys, evt_cnt);
        void(_svs); void(_svs_tys);
        const fake_res = {
          didSend: true,
          didTimeout: false,
          data: msg,
          time: curTime,
          secs: curSecs,
          value: value,
          from: pks,
          getOutput: (async <X extends CBR_Val>(o_mode:string, o_lab:string, o_ctc:ALGO_Ty<X>, o_val:X): Promise<X> => {
            void(o_mode);
            void(o_lab);
            // When user doesn't provide remote().ALGO({ simReturnVal: ... }), it gets turned
            // into undefined. Turn it back into a sensible default value.
            return o_val !== undefined ? o_val : o_ctc.defaultValue;
          }),
        };
        const sim_r = await sim_p( fake_res );
        debug(dhead , '--- SIMULATE', sim_r);
        if ( isCtor ) {
          const amt =
            hasCompanion ?
              minimumBalance.mul(2) :
              minimumBalance;
          sim_r.txns.unshift({
            kind: 'to',
            amt: amt,
            tok: undefined,
          });
        }
        const { isHalt } = sim_r;

        // Maps
        if ( hasMaps ) { await ensureOptIn(); }
        const { mapRefs } = sim_r;

        while ( true ) {
          const params = await getTxnParams(dhead);
          // We add one, because the firstRound field is actually the current
          // round, which we couldn't possibly be in, because it already
          // happened.
          debug(dhead, '--- TIMECHECK', { params, timeoutAt });
          if ( await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, bigNumberify(params.firstRound).add(1) ) ) {
            return await doRecv(false, false, `timeout`);
          }
          if ( ! soloSend && ! await canIWin(lct) ) {
            return await doRecv(false, false, `cannot win ${lct}`);
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

          const foreignArr: Array<number> = [ ];
          const recordApp = (app:ContractInfo) => {
            const appn = bigNumberToNumber(app);
            if ( ! foreignArr.includes(appn) ) {
              foreignArr.push(appn);
            }
          };
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
          let whichApi : string|undefined;
          const processRemote = (dr: SimRemote) => {
            dr.toks.map(recordAsset);
            dr.accs.map(recordAccount);
            dr.apps.map(recordApp);
            howManyMoreFees +=
              1
              + bigNumberToNumber(dr.pays)
              + bigNumberToNumber(dr.bills)
              + bigNumberToNumber(dr.fees);
            return;
          }
          const processSimTxn = (t: SimTxn) => {
            let txn;
            if ( t.kind === 'contractNew' ) {
              processSimTxn({
                kind: 'to',
                amt: minimumBalance_app_create(t.cns[connector]),
                tok: undefined,
              });
              processRemote(t.remote);
              return;
            } else if ( t.kind === 'tokenNew' ) {
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
            } else if ( t.kind === 'tokenAccepted' ) {
              recordAsset(t.tok);
              recordAccount(t.addr);
              return;
            } else if ( t.kind === 'remote' ) {
              recordApp(t.obj);
              processRemote(t.remote);
              return;
            }  else if ( t.kind === 'api' ) {
              whichApi = t.who;
              return;
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
                if ( tok ) { recordAsset(tok); }
                recordAccount_(Deployer);
                howManyMoreFees++; return;
              } else if ( t.kind === 'to' ) {
                from = thisAcc.addr;
                to = ctcAddr;
                amt = t.amt;
              } else if ( t.kind === 'info' ) {
                recordAsset(tok);
                return;
              } else {
                assert(false, 'sim txn kind');
              }
              txn = makeTransferTxn(from, to, amt, tok, params, closeTo, sim_i++);
            }
            extraFees += txn.fee;
            txn.fee = 0;
            txnExtraTxns.push(txn);
          };
          sim_r.txns.forEach(processSimTxn);
          if ( hasCompanion ) {
            if ( isCtor ) {
              // XXX Algorand says I won't need this eventually
              recordApp(bigNumberify(0));
              howManyMoreFees++;
            }
            const addCompanion = () => {
              if ( ! isCtor ) {
                if ( companionApp === undefined ) {
                  throw Error('impossible: no companion yet');
                }
                recordApp(companionApp);
              }
            };
            const readCI = (lab:string) => companionInfo[lab]||0;
            const companionCalls = readCI(`publish${funcNum}`) + (whichApi ? readCI(`api_${whichApi}`) : 0);
            debug('companion', { whichApi, companionCalls, companionInfo });
            if ( companionCalls > 0 ) {
              howManyMoreFees += companionCalls;
              addCompanion();
            }
            if ( isHalt ) {
              addCompanion();
              howManyMoreFees++;
            }
          }
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

          const foreignVal: number[]|undefined =
            (foreignArr.length === 0) ? undefined : foreignArr;
          debug(dhead, {foreignArr, foreignVal});

          const actual_args = [ lct, msg ];
          const actual_tys = [ T_UInt, T_Tuple(msg_tys) ];
          debug(dhead, '--- ARGS =', actual_args);

          const safe_args: Array<NV> = actual_args.map(
            // @ts-ignore
            (m, i) => actual_tys[i].toNet(m));
          safe_args.unshift(new Uint8Array([funcNum]));
          safe_args.unshift(new Uint8Array([0]));
          safe_args.forEach((x) => {
            if (! ( x instanceof Uint8Array ) ) {
              // The types say this is impossible now,
              // but we'll leave it in for a while just in case...
              throw Error(`expect safe program argument, got ${j2s(x)}`);
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
              thisAcc.addr, params, bigNumberToNumber(ApplicationID), safe_args,
              mapAcctsVal, foreignVal, assetsVal, NOTE_Reach);
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
            const jes = j2s(e);
            debug(dhead, 'FAIL', e, jes);

            if ( ! soloSend ) {
              // If there is no soloSend, then someone else "won", so let's
              // listen for their message
              debug(dhead, 'LOST');
              return await doRecv(false, false, jes);
            }

            if ( timeoutAt ) {
              // If there can be a timeout, then keep waiting for it
              debug(dhead, `CONTINUE`);
              continue;
            } else {
              // Otherwise, something bad is happening
              throw Error(`${label} failed to call ${funcName}: ${jes}`);
            }
          }

          debug(dhead, 'SUCCESS', res);
          return await trustedRecv(res);
        }
      };

      type RecvFromArgs = {
        dhead: string,
        out_tys: Array<ConnectorTy>,
        didSend: boolean,
        funcNum: number,
        txn: RecvTxn,
      };
      const recvFrom = async (rfargs:RecvFromArgs): Promise<Recv> => {
        const { dhead, funcNum, out_tys, didSend, txn } = rfargs;
        debug(dhead, 'txn', txn);
        if ( hasCompanion ) {
          const isCtor = funcNum === 0;
          if ( isCtor ) {
            const ccai = txn['created-companion-application-index'];
            if ( ccai == undefined ) {
              throw Error('impossible: no companion index');
            }
            companionApp = bigNumberify(ccai);
          }
        }
        const theRound = txn['confirmed-round'];
        // const theSecs = txn['round-time'];
        // ^ The contract actually uses `global LatestTimestamp` which is the
        // time of the PREVIOUS round.
        // ^ Also, this field is only available from the indexer
        const theSecs = await retryLoop([dhead, 'getTimeSecs'], () => getTimeSecs(bigNumberify(theRound)));
        // ^ XXX it would be nice if Reach could support variables bound to
        // promises and then we wouldn't need to wait here.

        const lr = makeLogRep(reachEvent(funcNum), out_tys);
        const ctc_args = lr.parse0(txn);
        debug(dhead, {ctc_args});
        if ( ctc_args === undefined ) {
          throw Error(`impossible: txn doesn't have right log as first`);
        }

        const fromAddr = txn['sender'];
        const from = T_Address.canonicalize({addr: fromAddr});
        debug(dhead, { from, fromAddr });

        const getOutput = async <X extends CBR_Val>(o_mode:string, o_lab:string, o_ctc:ALGO_Ty<X>, o_val:X): Promise<X> => {
          debug(`getOutput`, {o_mode, o_lab, o_ctc, o_val});
          const f_ctc = T_Tuple([T_UInt, o_ctc]);
          for ( const l of txn['logs'] ) {
            const lb = reNetify(l);
            const ln = T_UInt.fromNet(lb);
            const ls = `v${ln}`;
            debug(`getOutput`, {l, lb, ln, ls});
            if ( ls === o_lab ) {
              const ld = f_ctc.fromNet(lb);
              const o = ld[1] as X;
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
          time: bigNumberify(theRound),
          secs: bigNumberify(theSecs),
          from, getOutput,
        };
      };

      const recv = async (rargs:RecvArgs): Promise<Recv> => {
        const { funcNum, out_tys, didSend, timeoutAt, waitIfNotPresent } = rargs;
        const funcName = `m${funcNum}`;
        const dhead = `${label}: recv ${funcName} ${timeoutAt}`;
        debug(dhead, 'start');
        const { isIsolatedNetwork } = await getC();
        const didTimeout = async (cr_bn: BigNumber): Promise<boolean> => {
          const crp = cr_bn.add(1);
          debug(dhead, 'TIMECHECK', {timeoutAt, cr_bn, crp});
          const r = await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, crp);
          debug(dhead, 'TIMECHECK', {r, waitIfNotPresent});
          if ( !r && waitIfNotPresent ) {
            await waitUntilTime(crp);
          }
          return r;
        };
        const res = await eq.peq(dhead, didTimeout);
        debug(dhead, `res`, res);
        const correctStep = makeHasLogFor(funcNum, out_tys);
        const good = (! res.timeout) && correctStep(res.txn);
        if ( good ) {
          await eq.deq(dhead);
          const txn = res.txn;
          return await recvFrom({dhead, out_tys, didSend, funcNum, txn});
        } else if ( timeoutAt ) {
          debug(dhead, `timeout`);
          return { didTimeout: true };
        } else {
          throw Error(`${dhead}: impossible: not good, but no timeout`);
        }
      };

      const getBalance = async (mtok: Token|false = false) => {
        const { ctcAddr } = await getC();
        // @ts-ignore
        const bal = await balanceOf({ addr: ctcAddr }, mtok);
        // XXX May be wrong sometimes. Accurate minimumBalance requires this change:
        //     https://github.com/algorand/go-algorand/pull/3287
        const result = bal.lt(minimumBalance) ? bigNumberify(0) : bal.sub(minimumBalance);
        debug(`Balance of contract:`, result);
        return result;
      }

      return { getContractInfo, getContractAddress, getContractCompanion, getBalance, getState, getCurrentStep, sendrecv, recv, apiMapRef, simTokenAccepted };
    };

    const readStateBytes = (prefix:string, key:number[], src:AppStateKVs): (Uint8Array|undefined) => {
      debug({prefix, key});
      const ik = base64ify(new Uint8Array(key));
      debug({ik});
      const ste = src.find((x) => x.key === ik);
      debug({ste});
      if ( ste === undefined ) { return undefined; };
      const st = ste.value;
      debug({st});
      if ( st.bytes === undefined ) { return undefined; };
      const bsi = base64ToUI8A(st.bytes);
      debug({bsi});
      return bsi;
    };

    const recoverSplitBytes = (prefix:string, size:number, howMany:number, src:AppStateKVs): Uint8Array | undefined => {
      debug('recoverSplitBytes', {prefix, size, howMany, src});
      const bs = new Uint8Array(size);
      let offset = 0;
      for ( let i = 0; i < howMany; i++ ) {
        const bsi = readStateBytes(prefix, [i], src);
        if ( !bsi || bsi.length == 0 ) {
          // We are at a state where we don't need all the keys, so they
          // haven't all been set, bs is initialized to 0, so this should be
          // fine.
          return bs;
        }
        bs.set(bsi, offset);
        offset += bsi.length;
      }
      return bs;
    };
    const setupView = (setupViewArgs: SetupViewArgs) => {
      const eq = newEventQueue();
      const getC = makeGetC(setupViewArgs, eq);
      const viewLib: IViewLib = {
        viewMapRef: async (mapi: number, a:Address): Promise<any> => {
          const { viewMapRef } = await getC();
          return await viewMapRef(mapi, a);
        },
      };
      const getView1 = (vs:BackendViewsInfo, v:string, k:string|undefined, vim: BackendViewInfo, isSafe = true) =>
        async (...args: any[]): Promise<any> => {
          debug('getView1', v, k, args);
          const { decode } = vim;
          const ch = await getC();
          try {
            const step = await getCurrentStep_(ch);
            const vi = bigNumberToNumber(step);
            const vtys = vs[vi];
            if (!vtys) { throw Error(`no views for state ${step}`); }
            const vvs = (await getState_(getC, _ => vtys))[1];
            const vres = await decode(vi, vvs, args);
            debug({ vres });
            return isSafe ? ['Some', vres] : vres;
          } catch (e) {
            debug(`getView1`, v, k, 'error', e);
            if (!isSafe) { throw Error(`View ${v}.${k} is not set.`); }
            return ['None', null];
          }
      };
      return { getView1, viewLib };
    };

    const setupEvents = (setupArgs: SetupEventArgs) => {
      const createEventStream = (evt: string, tys: AnyALGO_Ty[]) => {
        const eq = newEventQueue();
        const getC = makeGetC(setupArgs, eq);
        const getTxnTime = (r:RecvTxn) => bigNumberify(r['confirmed-round']);
        const sync = async () => {
          const {} = await getC();
          return;
        };
        const getLogs = (r:RecvTxn) => r['logs'];
        const lr = makeLogRep(evt, tys);
        const parseLog = lr.parse;
        return makeEventStream<EQInitArgs, IndexerTxn, RecvTxn, string>({
          eq, getTxnTime, sync, getNetworkTime, getLogs, parseLog,
        });
      };
      return { createEventStream };
    };

    const { sigs: ABI_sigs } = ABI;
    const getABI = (isFull?:boolean) => ({
      sigs: (isFull ? ABI_sigs : ABI_sigs.map((name:string) => ({name})).filter(stdABIFilter).map(({name}:{name: string}) => name)),
    });
    const getEventTys = mkGetEventTys(bin, stdlib);

    const doAppOptIn = async (ctc: ContractInfo) => {
      return await doAccountAppOptIn(networkAccount, ctc);
    }

    return stdContract({ bin, getABI, getEventTys, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, setupEvents, _setup, givenInfoP, doAppOptIn });
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    debug(`setDebugLabel`, { newLabel, pks });
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

  const tokensAccepted_ = (): Promise<Array<Token>> => tokensAccepted(extractAddr(networkAccount));

  const tokenAccept = async (token:Token): Promise<void> => {
    if ( ! (await tokenAccepted(token)) ) {
      debug(`tokenAccept`, token);
      // @ts-ignore
      await transfer(me_na, me_na, 0, token);
    }
  };

  const tokenMetadata = async (token:Token): Promise<TokenMetadata> => {
    debug(`tokenMetadata`, token);
    const tokenRes = await getAssetInfo(bigNumberToNumber(token));
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
    const { clawback, creator, freeze, manager, reserve } = tokenInfo;
    const defaultFrozen = tokenInfo['default-frozen'];
    return { name, symbol, url, metadata, supply, decimals, clawback, creator, defaultFrozen, freeze, manager, reserve };
  };
  const unsupportedAcc = stdAccount_unsupported(connector);

  const accObj = { ...unsupportedAcc, networkAccount, getAddress: selfAddress,
                   stdlib, setDebugLabel, tokenAccepted, tokensAccepted: tokensAccepted_,
                   tokenAccept, tokenMetadata, contract };
  const acc = accObj as unknown as Account;
  const balanceOf_ = (token?: Token): Promise<BigNumber> => balanceOf(acc, token);
  const balancesOf_ = (tokens: Array<Token | null>): Promise<Array<BigNumber>> => balancesOf(acc, tokens);
  const appOptedIn = async (ctc: ContractInfo): Promise<boolean> => await accountAppOptedIn(acc, ctc);

  return stdAccount({
    ...accObj, balanceOf: balanceOf_, balancesOf: balancesOf_, appOptedIn,
  });
};

const tokensAccepted = async (acc: Account | Address): Promise<Array<Token>> => {
  const addr = extractAddrConvert(acc);
  let assetHoldings: Array<AssetHolding>;
  if (await nodeCanRead()) {
    const accountInfo = await getAccountInfo(addr);
    assetHoldings = accountInfo['assets'] ?? [];
  } else {
    const indexer = await getIndexer();
    const query = indexer.lookupAccountAssets(addr) as unknown as ApiCall<IndexerAccountAssetsRes>;
    assetHoldings = (await doQuery_('tokensAccepted', query))['assets'];
  }
  return assetHoldings.map(ah => bigNumberify(ah["asset-id"]));
}

const minimumBalanceOf = async (acc: Account | Address): Promise<BigNumber> => {
  const ai = await getAccountInfo(acc);
  if ( ai.amount === BigInt(0) ) { return bigNumberify(0); }
  const createdAppCount = bigNumberify((ai['created-apps']??[]).length);
  const optinAppCount = bigNumberify((ai['apps-local-state']??[]).length);
  const numByteSlice = bigNumberify((ai['apps-total-schema']??{})['num-byte-slice']??0);
  const numUInt = bigNumberify((ai['apps-total-schema']??{})['num-uint']??0);
  const assetCount = bigNumberify((ai.assets??[]).length)
  const accMinBalance = bigNumberify(0)
    .add(assetCount.mul(appFlatOptInMinBalance))
    .add(schemaMinBalancePerEntry.add(schemaUintMinBalance).mul(numUInt))
    .add(schemaMinBalancePerEntry.add(schemaBytesMinBalance).mul(numByteSlice))
    .add(appFlatParamsMinBalance.mul(createdAppCount))
    .add(appFlatOptInMinBalance.mul(optinAppCount))
    .add(minimumBalance);
  debug(`minBalance`, accMinBalance);
  return accMinBalance;
};

const balancesOfM = async (acc: Account | Address, tokens: Array<Token|null>): Promise<Array<BigNumber|false>> => {
  const bn = bigNumberify;
  const tokenbs = tokens.map((tr) => tr == null ? tr : bn(tr));
  const addr = extractAddrConvert(acc);

  // Querying Algod for balances
  if (await nodeCanRead()) {
    const client = await getAlgodClient();
    const query = client.accountInformation(addr) as unknown as ApiCall<AccountInfo>;
    const accountInfoM = await doQueryM_('balancesOfM', query);
    if ('val' in accountInfoM) {
      const accountInfo = accountInfoM['val'];
      const accountAssets = accountInfo['assets'] ?? [];
      const tokenBalances = tokenbs.map(tb => {
        if (tb === null) {
          return bn(accountInfo['amount']);
        } else {
          const bal = accountAssets.find(asset => tb.eq(asset['asset-id']))?.['amount'];
          return bal ? bn(bal) : false;
        }
      });
      return tokenBalances;
    }
  }

  // If Algod failed, (eg if query response exceeded MaxAPIResourcesPerAccount), query the indexer
  const indexer = await getIndexer();
  const query = indexer.lookupAccountAssets(addr) as unknown as ApiCall<IndexerAccountAssetsRes>;
  const accountAssets = (await doQuery_('balancesOfM', query))['assets'];
  const tokenBalances = tokenbs.map(async tb => {
    if (tb === null) {
      return await balanceOfM(acc, tb);
    } else {
      const bal = accountAssets.find(asset => tb.eq(asset['asset-id']))?.['amount'];
      return bal ? bn(bal) : false;
    }
  });
  return Promise.all(tokenBalances);
};

const balanceOfM = async (acc: Account | Address, token: Token | null): Promise<BigNumber | false> => {
  const dhead = 'balanceOfM';
  const addr = extractAddrConvert(acc);
  if (token == null) {
    if (await nodeCanRead()) {
      const client = await getAlgodClient();
      // Node algo balance
      const query = client.accountInformation(addr).exclude('all') as unknown as ApiCall<AccountInfo>;
      const accountInfo = await doQuery_(dhead, query);
      return bigNumberify(accountInfo['amount']);
    } else {
      const indexer = await getIndexer();
      // Indexer algo balance
      const query = indexer.lookupAccountByID(addr).exclude('all') as unknown as ApiCall<IndexerAccountInfoRes>;
      const accountInfoM = await doQueryM_(dhead, query);
      if ('val' in accountInfoM) {
        return bigNumberify(accountInfoM['val']['account']['amount']);
      } else {
        return false;
      }
    }
  } else {
    return balanceOfMNonNet(addr, token);
  }
}

const balanceOfMNonNet = async (addr:string, token:Token) => {
  const tokenId = bigNumberToNumber(token);
  const client = await getAlgodClient();
  const query = client.accountAssetInformation(addr, tokenId) as ApiCall<AccountAssetInfo>;
  const accountAssetInfoM = await doQueryM_("balanceOfMNonNet", query);
  if ('val' in accountAssetInfoM) {
    const assetHolding = accountAssetInfoM['val']['asset-holding'];
    return assetHolding ? bigNumberify(assetHolding['amount']) : false;
  } else {
    return false;
  }
}

const balancesOf = async (acc: Account | Address, tokens: Array<Token|null>): Promise<Array<BigNumber>> => {
  return (await balancesOfM(acc, tokens)).map(bal => bal == false ? bigNumberify(0) : bal);
};

const balanceOf = async (acc: Account | Address, token?: Token): Promise<BigNumber> => {
  return (await balanceOfM(acc, token || null)) || bigNumberify(0);
};

const doAccountAppOptedIn = async (nacc: Address, ctcId: ContractInfo): Promise<boolean> => {
  const ls = await getLocalState_(nacc, ctcId);
  return ls !== undefined;
}
const accountAppOptedIn = async (acc: Account | Address, ctc: ContractInfo): Promise<boolean> => {
  const addr = extractAddrConvert(acc);
  return await doAccountAppOptedIn(addr, ctc);
}
const doAccountAppOptIn = async (nacc: NetworkAccount, ctcId: ContractInfo): Promise<void> => {
  if (!(await doAccountAppOptedIn(nacc.addr, ctcId))) {
    const dhead = "accountAppOptIn";
    await sign_and_send_sync(
      dhead,
      nacc,
      toWTxn(algosdk.makeApplicationOptInTxn(
        nacc.addr, await getTxnParams(dhead),
        bigNumberToNumber(ctcId),
        undefined, undefined, undefined, undefined,
        NOTE_Reach)));
    // We are commenting this out because the above ^ might not be
    // propagated to Indexer on the CI fast enough.
    // assert(await accountAppOptedIn(acc, ctc), `didOptIn after doOptIn`);
  }
};

const createAccount = async (): Promise<Account> => {
  const networkAccount = algosdk.generateAccount();
  return await connectAccount(networkAccount);
};

const canFundFromFaucet = async (): Promise<boolean> => {
  const faucet = await getFaucet();
  const dhead = 'canFundFromFaucet';
  debug(dhead, 'check genesis');
  const txnParams = await getTxnParams(dhead);
  const act = txnParams.genesisID;
  const exp = 'devnet-v1';
  if (act !== exp) {
    debug(dhead, `expected '${exp}' !== actual '${act}'`);
    return false;
  }
  debug(dhead, 'check balance');
  const fbal = await balanceOf(faucet);
  debug(dhead, `faucet balance = ${formatCurrency(fbal, 4)} ${standardUnit}`);
  return gt(fbal, 0);
};

const fundFromFaucet = async (acc: Account | Address, value: unknown) => {
  if (! hideWarnings()){
    console.error("Warning: your program uses stdlib.fundFromFaucet. That means it only works on Reach devnets!");
  }
  const faucet = await getFaucet();
  debug('fundFromFaucet');
  const tag = Math.round(Math.random() * (2 ** 32));
  await transfer(faucet, acc, value, undefined, { tag });
};

const newTestAccount = async (startingBalance: unknown) => {
  const account = await createAccount();
  await fundFromFaucet(account, startingBalance);
  return account;
};

const newTestAccounts = make_newTestAccounts(newTestAccount).parallel;

/** @description the display name of the standard unit of currency for the network */
const standardUnit = 'ALGO';

/** @description the display name of the atomic (smallest) unit of currency for the network */
const atomicUnit = 'μALGO';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the token.
 * @param {number} [decimals] how many "decimal places" the target currency has. Defaults to the network standard.
 * @returns  the amount in the {@link atomicUnit} of the token.
 * @example  parseCurrency(100).toString() // => '100000000'
 * @example  parseCurrency(100, 3).toString() // => '100000'
 */
const parseCurrency = makeParseCurrency(6);

const minimumBalance: BigNumber =
  bigNumberify(MinBalance);

const schemaMinBalancePerEntry: BigNumber = bigNumberify(SchemaMinBalancePerEntry)
const schemaBytesMinBalance: BigNumber = bigNumberify(SchemaBytesMinBalance)
const schemaUintMinBalance: BigNumber = bigNumberify(SchemaUintMinBalance)
const appFlatParamsMinBalance: BigNumber = bigNumberify(AppFlatParamsMinBalance)
const appFlatOptInMinBalance: BigNumber = bigNumberify(AppFlatOptInMinBalance)

const minimumBalance_app_create = (cns:any): BigNumber => {
  const { code, opts } = cns;
  const { approvalB64, clearStateB64 } = code;
  const approval = Buffer.from(approvalB64, 'base64');
  const clearState = Buffer.from(clearStateB64, 'base64');
  const totalLen = approval.length + clearState.length;
  const ai_ExtraProgramPages = Math.ceil(totalLen / MaxAppProgramLen) - 1;
  const { globalBytes: ai_GlobalNumByteSlice, globalUints: ai_GlobalNumUint } = opts;
  return bigNumberify(100000*(1+ai_ExtraProgramPages) + (25000+3500)*ai_GlobalNumUint + (25000+25000)*ai_GlobalNumByteSlice);
};

/**
 * @description  Format currency by network
 */
function formatCurrency(amt: unknown, decimals: number = 6): string {
  return handleFormat(amt, decimals, 6)
}

async function getDefaultAccount(): Promise<Account> {
  const addr = await (await getProvider()).getDefaultAddress();
  return await connectAccount({ addr });
}

/**
 * @param mnemonic 25 words, whitespace-separated
 */
const newAccountFromMnemonic = (phrase: Mnemonic): Promise<Account> =>
  connectAccount(algosdk.mnemonicToSecretKey(protectMnemonic(phrase, 25)));

/**
 * @param secret a Uint8Array, or its hex string representation
 */
const newAccountFromSecret = (sk: SecretKeyInput): Promise<Account> =>
  newAccountFromMnemonic(algosdk.secretKeyToMnemonic(protectSecretKey(sk, 32)));

const getNetworkTime = async (): Promise<BigNumber> => {
  const indexer = await getIndexer();
  const hc = await indexer.makeHealthCheck().do();
  return bigNumberify(hc['round']);
};
const getTimeSecs = async (now_bn: BigNumber): Promise<BigNumber> => {
  const now = bigNumberToNumber(now_bn);
  try {
    await ensureNodeCanRead();
    const client = await getAlgodClient();
    const binfo = await client.block(now).do();
    //debug(`getTimeSecs`, `node`, binfo);
    return bigNumberify(binfo.block.ts);
  } catch (e:any) {
    debug(`getTimeSecs`, `node failed`, e);
    const indexer = await getIndexer();
    const info = await indexer.lookupBlock(now).do();
    debug(`getTimeSecs`, `indexer`, info);
    return bigNumberify(info['timestamp']);
  }
};
const getNetworkSecs = async (): Promise<BigNumber> =>
  await getTimeSecs(await getNetworkTime());

const stepTime = async (target: BigNumber): Promise<BigNumber> => {
  while ( true ) {
    const now = await getNetworkTime();
    debug(`stepTime`, {target, now});
    if ( target.lte(now) ) { return now; }
    if ( (await getProvider()).isIsolatedNetwork ) {
      await fundFromFaucet(await getFaucet(), 0);
    } else {
      await stdWait();
    }
  }
};
const waitUntilTime = make_waitUntilX('time', getNetworkTime, stepTime);

const stepSecs = async (target: BigNumber): Promise<BigNumber> => {
  void(target);
  const now = await stepTime((await getNetworkTime()).add(1));
  return await getTimeSecs(now);
};
const waitUntilSecs = make_waitUntilX('secs', getNetworkSecs, stepSecs);

const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  return await waitUntilTime(now.add(delta), onProgress);
};

const appLocalStateNumUInt = 0;
const appLocalStateNumBytes = 0;
const appGlobalStateNumUInt = 0;
const appGlobalStateNumBytes = 1;

const verifyContract = async (info: ContractInfo, bin: Backend): Promise<VerifyResult> => {
  return verifyContract_('', info, bin, newEventQueue());
}

const verifyContract_ = async (label:string, info: ContractInfo, bin: Backend, eq: EventQueue): Promise<VerifyResult> => {
  must_be_supported(bin);
  // @ts-ignore
  const ApplicationID: BigNumber = protect(T_Contract, info);
  const { appApproval, appClear, mapDataKeys, stateKeys } =
    bin._Connectors.ALGO;

  let dhead = `${label}: verifyContract`;

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`${dhead} failed: ${msg}`);
    }
  };
  const chkeq_x = <X>(cmp:((a:X, b:X) => boolean)) => (a: X, e:X, msg:string) => {
    const as = j2sf(a);
    const es = j2sf(e);
    chk(cmp(a, e), `${msg}: expected ${es}, got ${as}`);
  };
  const chkeq_bn_ = chkeq_x<BigNumber>((a:BigNumber, b:BigNumber) => a.eq(b));
  const chkeq_bn = (a:unknown, b:unknown, msg:string) => chkeq_bn_(bigNumberify(a), bigNumberify(b), msg);
  type Program = string|undefined;
  const chkeq_bs = chkeq_x<Program>((a:Program, b:Program) => j2sf(a) === j2sf(b));

  const appInfoM = await getApplicationInfoM(ApplicationID);
  if ('exn' in appInfoM) {
    throw Error(`${dhead} failed: failed to lookup application (${ApplicationID}): ${j2s(appInfoM.exn)}`);
  }

  const appInfo = appInfoM.val;
  const appInfo_p = appInfo['params'];
  debug(dhead, {appInfo_wanted: bin._Connectors.ALGO});
  debug(dhead, {appInfo_p});
  chk(appInfo_p !== undefined, `Cannot lookup ApplicationId`);
  chkeq_bs(appInfo_p['approval-program'], appApproval, `Approval program does not match Reach backend`);
  chkeq_bs(appInfo_p['clear-state-program'], appClear, `ClearState program does not match Reach backend`);
  const Deployer = appInfo_p['creator'];

  const appInfo_LocalState = appInfo_p['local-state-schema'];
  chkeq_bn(appInfo_LocalState['num-byte-slice'], appLocalStateNumBytes + mapDataKeys, `Num of byte-slices in local state schema does not match Reach backend`);
  chkeq_bn(appInfo_LocalState['num-uint'], appLocalStateNumUInt, `Num of uints in local state schema does not match Reach backend`);

  const appInfo_GlobalState = appInfo_p['global-state-schema'];
  chkeq_bn(appInfo_GlobalState['num-byte-slice'], appGlobalStateNumBytes + stateKeys, `Num of byte-slices in global state schema does not match Reach backend`);
  chkeq_bn(appInfo_GlobalState['num-uint'], appGlobalStateNumUInt, `Num of uints in global state schema does not match Reach backend`);

  eq.init({ ApplicationID });

  // Next, we check that it was created with this program and wasn't created
  // with a different program first (which could have modified the state)
  const iat = await eq.deq(dhead, 1);
  debug({iat});
  chkeq_bn(iat['created-application-index'], ApplicationID, 'app created');
  chkeq_bn(iat['application-index'], 0, 'app created');
  const allocRound = appInfo['created-at-round'];
  if ( allocRound ) {
    chkeq_bn(iat['confirmed-round'], allocRound, 'created on correct round');
  }
  chkeq_bs(iat['approval-program'], appInfo_p['approval-program'], `ApprovalProgram unchanged since creation`);
  chkeq_bs(iat['clear-state-program'], appInfo_p['clear-state-program'], `ClearStateProgram unchanged since creation`);

  return { ApplicationID, Deployer };
};

/**
 * Formats an account's address in the way users expect to see it.
 * @param acc Account, NetworkAccount, base32-encoded address, or hex-encoded address
 * @returns the address formatted as a base32-encoded string with checksum
 */
function formatAddress(acc: string|NetworkAccount|Account|Address): string {
  return addressFromHex(T_Address.canonicalize(acc));
}

function unsafeGetMnemonic(acc: NetworkAccount|Account): string {
  // @ts-ignore
  const networkAccount: NetworkAccount = acc.networkAccount || acc;
  if (!networkAccount.sk) { throw Error(`unsafeGetMnemonic: Secret key not accessible for account`); }
  return algosdk.secretKeyToMnemonic(networkAccount.sk);
}

const launchToken = async (accCreator: Account, name: string, sym: string, opts: LaunchTokenOpts = {}) => {
  const addrCreator = accCreator.networkAccount.addr;
  const supply = opts.supply ? bigNumberify(opts.supply) : bigNumberify(2).pow(64).sub(1);
  const decimals = opts.decimals ?? 6;
  const url = opts.url ?? '';
  const assetMetadataHash = opts.metadataHash ?? '';
  const f_addr = (x:unknown) => x ? cbr2algo_addr(protect(T_Address, x) as string) : undefined;
  const clawback = f_addr(opts.clawback);
  const freeze = f_addr(opts.freeze);
  const reserve = f_addr(opts.reserve);
  const defaultFrozen = opts.defaultFrozen ?? false;
  const suggestedParams = await getTxnParams('launchToken');

  const txnResult = await sign_and_send_sync(
    `launchToken ${j2s(accCreator)} ${name} ${sym}`,
    accCreator.networkAccount,
    toWTxn(algosdk.makeAssetCreateTxnWithSuggestedParamsFromObject({
      assetMetadataHash, assetName: name, assetURL: url, clawback,
      decimals, defaultFrozen, freeze, note: opts.note, reserve,
      suggestedParams, total: bigNumberToBigInt(supply), unitName: sym,
      from: addrCreator,
    }))
  );

  const assetIndex = txnResult['created-asset-index'];
  if (!assetIndex) throw Error(`${sym} no asset-index!`);
  const id = bigNumberify(assetIndex);

  const mint = (to: Account | Address, amt: any) => transfer(accCreator, to, amt, id);
  const optOut = async (accFrom: Account, accTo: Account = accCreator) => {
    // Opting out = sending all of your current balance to accTo
    const addrFrom = accFrom.networkAccount.addr;
    const addrTo = accTo.networkAccount.addr;
    const params = await getTxnParams('token.optOut');
    const optOutTxn = makeTransferTxn(addrFrom, addrTo, bigNumberify(0), id, params, addrTo);
    await sign_and_send_sync(
      `token.optOut ${j2s(accFrom)} ${name}`,
      accFrom.networkAccount,
      toWTxn(optOutTxn),
    );
  }

  return { name, sym, id, mint, optOut };
}

  return stdlibShared({
    ...stdlib,
    ...typeDefs,
    ...shared_user,
    ...({ setQueryLowerBound, getQueryLowerBound, addressFromHex, formatWithDecimals, setSigningMonitor }),
    setCustomHttpEventHandler,
    setMinMillisBetweenRequests,
    signSendAndConfirm, toWTxn, getTxnParams, MinTxnFee, makeTransferTxn,
    getValidQueryWindow, setValidQueryWindow,
    randomUInt, hasRandom,
    setWalletFallback, walletFallback,
    getProvider, setProvider, setProviderByEnv, setProviderByName,
    getFaucet, setFaucet, canFundFromFaucet, fundFromFaucet,
    providerEnvByName,
    transfer, connectAccount, minimumBalanceOf, balancesOf, balanceOf,
    appOptedIn: accountAppOptedIn,
    createAccount, newTestAccount, newTestAccounts, getDefaultAccount,
    newAccountFromMnemonic, newAccountFromSecret,
    getNetworkTime, getTimeSecs, getNetworkSecs,
    waitUntilTime, waitUntilSecs, wait, verifyContract,
    formatAddress, unsafeGetMnemonic, launchToken,
    parseCurrency, minimumBalance, formatCurrency,
    reachStdlib, algosdk,
    connector, standardUnit, atomicUnit,
    tokensAccepted
  });
};
