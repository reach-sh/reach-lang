export const connector = 'ALGO';

import algosdk from 'algosdk';
export { default as algosdk } from 'algosdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import buffer from 'buffer';
import type MyAlgoConnect from '@randlabs/myalgo-connect';
import type { Transaction, EncodedTransaction } from 'algosdk'; // =>
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
  ISimTxn,
  stdContract, stdVerifyContract,
  stdABIFilter,
  stdAccount,
  debug, envDefault,
  argsSplit,
  makeRandom,
  replaceableThunk,
  ensureConnectorAvailable,
  bigNumberToBigInt,
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
  bytestringyNet,
} from './ALGO_compiled';
export type { Token } from './ALGO_compiled';
import type { MapRefT, MaybeRep } from './shared_backend'; // =>
import { window, process, Env } from './shim';
import { sha512_256 } from 'js-sha512';
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = stdlib;
export * from './shared_user';
import { setQueryLowerBound, getQueryLowerBound } from './shared_impl';
export { setQueryLowerBound, getQueryLowerBound, addressFromHex };

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
  'created-asset-index'?: number,
  'created-application-index'?: number,
  'application-index'?: number,
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

const reachBackendVersion = 9;
const reachAlgoBackendVersion = 9;
export type Backend = IBackend<AnyALGO_Ty> & {_Connectors: {ALGO: {
  version: number,
  ABI: any,
  appApproval: string,
  appClear: string,
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

export type ContractInfo = number;
type SendRecvArgs = ISendRecvArgs<Address, Token, AnyALGO_Ty>;
type RecvArgs = IRecvArgs<AnyALGO_Ty>;
type Recv = IRecv<Address>
export type Contract = IContract<ContractInfo, Address, Token, AnyALGO_Ty>;
export type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
type SimTxn = ISimTxn<Token>
type SetupArgs = ISetupArgs<ContractInfo, VerifyResult>;
type SetupViewArgs = ISetupViewArgs<ContractInfo, VerifyResult>;
type SetupEventArgs = ISetupEventArgs<ContractInfo, VerifyResult>;
type SetupRes = ISetupRes<ContractInfo, Address, Token, AnyALGO_Ty>;

type AccountAssetInfo = {
  'asset-id': number,
  'amount': number,
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
  'id': number,
  'key-value': AppStateKVs,
};
type AppSchema = {
  "num-byte-slice": number,
  "num-uint": number
}
type AccountInfo = {
  'amount': number,
  'assets'?: Array<AccountAssetInfo>,
  'apps-local-state'?: Array<AppState>
  'apps-total-schema'?: AppSchema,
  'created-apps'?: Array<AppInfo>
};
type IndexerAccountInfoRes = {
  'current-round': number,
  'account': AccountInfo,
};

type AppStateSchema = {
  'num-uint': number,
  'num-byte-slice': number,
};
type AppInfo = {
  'id': number,
  'created-at-round': number,
  'deleted': boolean,
  'params': {
    'creator': string,
    'approval-program': string,
    'clear-state-program': string,
    'global-state': AppStateKVs,
    'extra-program-pages': number,
    'local-state-schema': AppStateSchema,
    'global-state-schema': AppStateSchema,
  },
};
type IndexerAppInfoRes = {
  'current-round': number,
  'application': AppInfo,
};

type AssetInfo = {
  'index': number,
  'params': {
    'clawback': string,
    'creator': string,
    'decimals': number,
    'default-frozen': boolean,
    'freeze': string,
    'manager': string,
    'metadata-hash': string,
    'name': string,
    'name-b64': string,
    'reserve': string,
    'total': number,
    'unit-name': string,
    'unit-name-b64': string,
    'url': string,
    'url-b64': string,
  },
};
type IndexerAssetInfoRes = {
  'current-round': number,
  'asset': AssetInfo,
};

type OrExn<X> = { val: X } | {exn:any};
type IndexerAppTxn = {
  'approval-program'?: string,
  'clear-state-program'?: string,
  'application-id'?: number,
  'application-args'?: Array<string>,
  'on-completion'?: string,
};
type IndexerTxn = {
  'confirmed-round': number,
  'sender': Address,
  'created-asset-index'?: number,
  'created-application-index'?: number,
  'application-transaction'?: IndexerAppTxn,
  'logs'?: Array<string>,
  'tx-type': string,
};
type IndexerQuery1Res = {
  'current-round': number,
  'transaction': IndexerTxn,
};
type IndexerQueryMRes = {
  'current-round': number,
  'transactions': Array<IndexerTxn>,
};
type AlgodTxn = {
  'asset-index'?: number,
  'application-index'?: number,
  'confirmed-round'?: number,
  'logs'?: Array<string>,
  'txn': {
    'sig': Uint8Array,
    'txn': EncodedTransaction,
  },
  'pool-error': string,
};

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


const indexerTxn2RecvTxn = (txn:IndexerTxn): RecvTxn => {
  const ait: IndexerAppTxn = txn['application-transaction'] || {};
  const aargs = ait['application-args'] || [];
  const aidx = ait['application-id'] || 0;
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
      return {
        'confirmed-round': cr,
        'created-asset-index': info['asset-index'],
        // @ts-ignore
        'logs': uToS(l),
        'created-application-index': info['application-index'],
        'sender': txnFromAddress(dtxn),
        'application-args': uToS(dtxn.appArgs),
      };
    } else if ( info['pool-error'] === '' ) {
      debug(dhead, 'still in pool, trying again');
      return await checkAlgod();
    } else {
      throw Error(`waitForConfirmation: error confirming: ${JSON.stringify(info)}`);
    }
  };

  const checkIndexer = async (): Promise<RecvTxn> => {
    const indexer = await getIndexer();
    const q = indexer.lookupTransactionByID(txId) as unknown as ApiCall<IndexerQuery1Res>;
    const res = await doQuery_(dhead, q);
    debug(dhead, 'indexer', res);
    return indexerTxn2RecvTxn(res['transaction']);
  };

  // AlgoExplorer will NOT support this query, but I believe it will fail and
  // then fall back to the Indexer, which does work, so we're keeping it in for
  // optimization on local cases
  return await checkAlgod();
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

export const signSendAndConfirm = async (
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
  } catch (e:any) {
    const es = `${e}`;
    if ( 'response' in e ) {
      const r = e.response;
      if ( 'body' in r ) {
        e.response = r.body;
      } else if ( 'text' in r ) {
        e.response = r.text;
      } else {
        delete r.request;
        delete r.req;
      }
    }
    throw { type: 'signAndPost', e, es };
  }
  const N = txns.length - 1;
  const tN = decodeB64Txn(txns[N].txn);
  try {
    return await waitForConfirmation(tN.txID()); // tN.lastRound
  } catch (e) {
    const es = `${e}`;
    throw { type: 'waitForConfirmation', e, es };
  }
};

const encodeUnsignedTransaction = (t:Transaction): string => {
  return Buffer.from(algosdk.encodeUnsignedTransaction(t)).toString('base64');
};

export const toWTxn = (t:Transaction): WalletTransaction => {
  return {
    txn: encodeUnsignedTransaction(t),
    signers: [ txnFromAddress(t) ],
  };
};

// Backend
const stdWait = () => Timeout.set(1000);

export const getTxnParams = async (label: string): Promise<TxnParams> => {
  const dhead = `${label} fillTxn`;
  debug(dhead, `getting params`);
  const client = await getAlgodClient();
  while (true) {
    const params = await client.getTransactionParams().do();
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
    let es = JSON.stringify(e);
    if ( es === '{}' ) { es = `${e}`; };
    throw Error(`${label} txn failed:\n${JSON.stringify(txn)}\nwith:\n${es}`);
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
  if ( warnings.length > 0 ) {
    console.error(`This Reach application is dangerous to run on Algorand for the following reasons:\n${render(warnings)}`);
  }
  if ( unsupported.length > 0 ) {
    throw Error(`This Reach application is not supported on Algorand for the following reasons:\n${render(unsupported)}`);
  }
}

// Get these from stdlib
// const MaxTxnLife = 1000;
export const MinTxnFee = 1000;
const MaxAppTxnAccounts = 4;
const MinBalance = 100000;

const ui8h = (x:Uint8Array): string => Buffer.from(x).toString('hex');
const base64ToUI8A = (x:string): Uint8Array => Uint8Array.from(Buffer.from(x, 'base64'));
const base64ify = (x: WithImplicitCoercion<string>|Uint8Array): string => Buffer.from(x).toString('base64');

const format_failed_request = (e:any) => {
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

const doQueryM_ = async <T>(dhead:string, query: ApiCall<T>): Promise<OrExn<T>> => {
  try {
    const val = await query.do();
    debug(dhead, 'RESULT', val);
    return { val };
  } catch (exn:any) {
    return { exn };
  }
};

const doQuery_ = async <T>(dhead:string, query: ApiCall<T>, howMany: number = 0): Promise<T> => {
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
      debug(dhead, 'RETRYING', {e});
      howMany++;
    } else {
      return res.val;
    }
  }
};

export function getValidQueryWindow(): number|true {
  return true;
}
export function setValidQueryWindow(n: number|true): void {
  if (typeof n === 'number') {
    throw Error(`Only setValidQueryWindow(true) is supported on Algorand`);
  }
}

const isCreateTxn = (txn:IndexerTxn): boolean => {
  const at = txn['application-transaction'];
  return at ? at['application-id'] === 0 : false;
};
const emptyOptIn = (txn:IndexerTxn) => {
  const at = txn['application-transaction'];
  const ataa = at && at['application-args'] || [];
  return at ?
    (at['on-completion'] === 'optin' && ataa.length == 0)
    : false;
};
type EQInitArgs = {
  ApplicationID: number,
};
type EventQueue = IEventQueue<EQInitArgs, IndexerTxn, RecvTxn>;
const newEventQueue = (): EventQueue => {
  const getTxns = async (dhead:string, initArgs:EQInitArgs, ctime: BigNumber, howMany: number): Promise<EQGetTxnsR<IndexerTxn>> => {
    const { ApplicationID } = initArgs;
    const indexer = await getIndexer();
    const mtime = bigNumberToNumber(ctime) + 1;
    debug(dhead, { ctime, mtime });
    const query =
      indexer.searchForTransactions()
        .applicationID(ApplicationID)
        //.txType('appl')
        .minRound(mtime);
    const q = query as unknown as ApiCall<IndexerQueryMRes>
    const res = await doQuery_(dhead, q, howMany);
    const txns = res.transactions.filter((x:IndexerTxn) => x['tx-type'] === 'appl');
    const gtime = bigNumberify(res['current-round']);
    return { txns, gtime };
  };
  const getTxnTime = (x:IndexerTxn): BigNumber => bigNumberify(x['confirmed-round']);
  return makeEventQueue<EQInitArgs, IndexerTxn, RecvTxn>({
    raw2proc: indexerTxn2RecvTxn,
    alwaysIgnored: emptyOptIn,
    getTxns, getTxnTime,
  });
};

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

export interface Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  getDefaultAddress: () => Promise<Address>,
  isIsolatedNetwork: boolean,
  signAndPostTxns: (txns:WalletTransaction[], opts?: object) => Promise<unknown>,
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

export const setWalletFallback = (wf:() => unknown) => {
  if ( ! window.algorand ) { window.algorand = wf(); }
};
const doWalletFallback_signOnly = (opts:any, getAddr:() => Promise<string>, signTxns:(txns:string[]) => Promise<string[]>): ARC11_Wallet => {
  let p: Provider|undefined = undefined;
  const enableNetwork = async (eopts?:object) => {
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
  const enableAccounts = async (eopts?:object) => {
    void(eopts);
    const addr = await getAddr();
    return { accounts: [ addr ] };
  };
  const enable = async (eopts?:object) => {
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
  const signAndPostTxns = async (txns:WalletTransaction[], sopts?:object) => {
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
  return doWalletFallback_signOnly(opts, getAddr, signTxns);
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
  return doWalletFallback_signOnly(opts, getAddr, signTxns);
};
const walletFallback_WalletConnect = (WalletConnect:any, opts:object) => (): ARC11_Wallet => {
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
  const signAndPostTxns = async (txns:WalletTransaction[], opts?:object) => {
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
    ALGO_SERVER: `https://node.${RANDLABS_BASE}`,
    ALGO_PORT: '',
    ALGO_TOKEN: '',
    ALGO_INDEXER_SERVER: `https://algoindexer.${RANDLABS_BASE}`,
    ALGO_INDEXER_PORT: '',
    ALGO_INDEXER_TOKEN: '',
    REACH_ISOLATED_NETWORK: 'no',
  }
}

export type ProviderName = string;
export function providerEnvByName(pn: ProviderName): ProviderEnv {
  switch (pn) {
    case 'MainNet': return randlabsProviderEnv('MainNet');
    case 'TestNet': return randlabsProviderEnv('TestNet');
    case 'BetaNet': return randlabsProviderEnv('BetaNet');
    case 'randlabs/MainNet': return randlabsProviderEnv('MainNet');
    case 'randlabs/TestNet': return randlabsProviderEnv('TestNet');
    case 'randlabs/BetaNet': return randlabsProviderEnv('BetaNet');
    case 'LocalHost': return localhostProviderEnv;
    default: throw Error(`Unrecognized provider name: ${pn}`);
  }
}

export function setProviderByName(pn: ProviderName): void {
  return setProviderByEnv(providerEnvByName(pn));
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
const NOTE_Reach_tag = (tag:number|undefined) => tag ? str2note(NOTE_Reach_str + ` ${tag})`) : NOTE_Reach;

export const makeTransferTxn = (
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
  value: unknown,
  token: Token|undefined = undefined,
  tag: number|undefined = undefined,
): Promise<RecvTxn> => {
  const sender = from.networkAccount;
  const receiver = extractAddr(to);
  const valuebn = bigNumberify(value);
  const ps = await getTxnParams('transfer');
  const txn = toWTxn(makeTransferTxn(sender.addr, receiver, valuebn, token, ps, undefined, tag));

  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuebn}`,
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

const getAccountInfo = async (a:Address): Promise<AccountInfo> => {
  const dhead = 'getAccountInfo';
  try {
    const client = await getAlgodClient();
    const res = (await client.accountInformation(a).do()) as AccountInfo;
    debug(dhead, 'node', res);
    return res;
  } catch (e:any) {
    debug(dhead, 'node err', e);
  }
  const indexer = await getIndexer();
  const q = indexer.lookupAccountByID(a) as unknown as ApiCall<IndexerAccountInfoRes>;
  const res = await doQuery_(dhead, q);
  debug(dhead, res);
  return res.account;
};

const getAssetInfo = async (a:number): Promise<AssetInfo> => {
  const dhead = 'getAssetInfo';
  const indexer = await getIndexer();
  const q = indexer.lookupAssetByID(a) as unknown as ApiCall<IndexerAssetInfoRes>;
  const res = await doQuery_(dhead, q);
  debug(dhead, res);
  return res.asset;
};

const getApplicationInfoM = async (id:number): Promise<OrExn<AppInfo>> => {
  const dhead = 'getApplicationInfo';
  try {
    const client = await getAlgodClient();
    const res = (await client.getApplicationByID(id).do()) as AppInfo;
    debug(dhead, 'node', res);
    return { val: res };
  } catch (e:any) {
    debug(dhead, 'node err', e);
    if ( e?.response?.body?.message === 'application does not exist' ) {
      return { exn: e };
    }
  }
  const indexer = await getIndexer();
  const q = indexer.lookupApplications(id) as unknown as ApiCall<IndexerAppInfoRes>;
  const res = await doQueryM_(dhead, q);
  debug(dhead, 'indexer', res);
  return 'exn' in res ? res : { val: res.val.application };
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

    const { stateSize, stateKeys, mapDataKeys, mapDataSize, ABI } = bin._Connectors.ALGO;
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

        const ctcAddr = algosdk.getApplicationAddress(ApplicationID);
        debug(label, 'getC', { ctcAddr });

        // Read map data
        const getLocalState = async (a:Address): Promise<AppStateKVs|undefined> => {
          const ai = await getAccountInfo(a);
          debug(`getLocalState`, ai);
          const alss = ai['apps-local-state'] || [];
          const als = alss.find((x) => (x.id === ApplicationID));
          debug(`getLocalState`, als);
          return als ? als['key-value'] : undefined;
        };

        // Application Local State Opt-in
        const didOptIn = async (): Promise<boolean> =>
          ((await getLocalState(thisAcc.addr)) !== undefined);
        const doOptIn = async (): Promise<void> => {
          const dhead = 'doOptIn';
          debug(dhead);
          await sign_and_send_sync(
            dhead,
            thisAcc,
            toWTxn(algosdk.makeApplicationOptInTxn(
              thisAcc.addr, await getTxnParams(dhead),
              ApplicationID,
              undefined, undefined, undefined, undefined,
              NOTE_Reach)));
          // We are commenting this out because the above ^ might not be
          // propagated to Indexer on the CI fast enough.
          // assert(await didOptIn(), `didOptIn after doOptIn`);
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

        const getAppState = async (): Promise<AppStateKVs|undefined> => {
          const lab = `getAppState`;
          const appInfoM = await getApplicationInfoM(ApplicationID);
          if ( 'exn' in appInfoM ) {
            debug(lab, {e: appInfoM.exn});
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

    const getState_ = async (getC:GetC, lookup:((vibna:BigNumber) => AnyALGO_Ty[])): Promise<Array<any>> => {
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
      const { setInfo, setTrustedVerifyResult } = setupArgs;

      const eq = newEventQueue();
      const getC = makeGetC(setupArgs, eq);

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
        debug('getState');
        return await getState_(getC, (vibna:BigNumber) => {
          if ( vibne.eq(vibna) ) { return vtys; }
          throw Error(`Expected state ${vibne}, got ${vibna}`);
        });
      };

      const apiMapRef = (i:number, ty:AnyALGO_Ty): MapRefT<any> => async (f:string): Promise<MaybeRep<any>> => {
        void(ty);
        const { viewMapRef } = await getC();
        return await viewMapRef(i, f);
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
                NOTE_Reach, undefined, undefined, extraPages)));

          const ApplicationID = createRes['created-application-index'];
          if ( ! ApplicationID ) {
            throw Error(`No created-application-index in ${JSON.stringify(createRes)}`);
          }
          debug(label, `created`, {ApplicationID});
          const ctcInfo = ApplicationID;
          setTrustedVerifyResult({ ApplicationID, Deployer });
          setInfo(ctcInfo);
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
          getOutput: (async <X extends CBR_Val>(o_mode:string, o_lab:string, o_ctc:ALGO_Ty<X>, o_val:X): Promise<X> => {
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
          const params = await getTxnParams(dhead);
          // We add one, because the firstRound field is actually the current
          // round, which we couldn't possibly be in, because it already
          // happened.
          debug(dhead, '--- TIMECHECK', { params, timeoutAt });
          if ( await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, params.firstRound + 1) ) {
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
          safe_args.unshift(new Uint8Array([0]));
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
            const jes = JSON.stringify(es);

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
        const theRound = txn['confirmed-round'];
        // const theSecs = txn['round-time'];
        // ^ The contract actually uses `global LatestTimestamp` which is the
        // time of the PREVIOUS round.
        // ^ Also, this field is only available from the indexer
        const theSecs = await retryLoop([dhead, 'getTimeSecs'], () => getTimeSecs(bigNumberify(theRound - 0)));
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
          const cr = bigNumberToNumber(cr_bn);
          debug(dhead, 'TIMECHECK', {timeoutAt, cr_bn, cr});
          const crp = cr + 1;
          const r = await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, crp);
          debug(dhead, 'TIMECHECK', {r, waitIfNotPresent});
          if ( !r && waitIfNotPresent ) {
            await waitUntilTime(bigNumberify(crp));
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
        const result = bal.eq(0) ? bal : bal.sub(minimumBalance);
        debug(`Balance of contract:`, result);
        return result;
      }

      return { getContractInfo, getContractAddress, getBalance, getState, sendrecv, recv, apiMapRef };
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
    const recoverSplitBytes = (prefix:string, size:number, howMany:number, src:AppStateKVs): (Uint8Array|undefined) => {
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

    return stdContract({ bin, getABI, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, setupEvents, _setup, givenInfoP });
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
    return { name, symbol, url, metadata, supply, decimals };
  };

  return stdAccount({ networkAccount, getAddress: selfAddress, stdlib, setDebugLabel, tokenAccepted, tokenAccept, tokenMetadata, contract });
};

export const minBalance = async (acc: Account): Promise<BigNumber> => {
  const addr = extractAddr(acc);
  const ai = await getAccountInfo(addr);
  const createdApps = ai['created-apps']??[]
  const numByteSlice = (ai['apps-total-schema']??{})['num-byte-slice']??0
  const numUInt = (ai['apps-total-schema']??{})['num-uint']??0
  const assets = ai.assets??[]
  const mBal = assets.length * 100000
    + (25000 + 3500) * numUInt
    + (25000 + 25000) * numByteSlice
    + (100000) * createdApps.length
    + 100000
    debug(`minBalance`, mBal)
    return bigNumberify(mBal)
}

const balanceOfM = async (acc: Account, token: Token|false = false): Promise<BigNumber|false> => {
  const addr = extractAddr(acc);
  const info = await getAccountInfo(addr);
  debug(`balanceOf`, info);
  if ( ! token ) {
    return bigNumberify(info.amount);
  } else {
    for ( const ai of (info.assets || []) ) {
      if ( bigNumberify(token).eq(ai['asset-id']) ) {
        return bigNumberify(ai['amount']);
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

export const fundFromFaucet = async (account: Account, value: unknown) => {
  const faucet = await getFaucet();
  debug('fundFromFaucet');
  const tag = Math.round(Math.random() * (2 ** 32));
  await transfer(faucet, account, value, undefined, tag);
};

export const newTestAccount = async (startingBalance: unknown) => {
  const account = await createAccount();
  await fundFromFaucet(account, startingBalance);
  return account;
};

export const newTestAccounts = make_newTestAccounts(newTestAccount).parallel;

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'ALGO';

/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'μALGO';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the token.
 * @returns  the amount in the {@link atomicUnit} of the token.
 * @example  parseCurrency(100).toString() // => '100000000'
 * @example  parseCurrency(100, 3).toString() // => '100000'
 */
export function parseCurrency(amt: CurrencyAmount, decimals: number = 6): BigNumber {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // @ts-ignore
  const numericAmt: number =
    isBigNumber(amt) ? amt.toNumber()
    : typeof amt === 'string' ? parseFloat(amt)
    : typeof amt === 'bigint' ? Number(amt)
    : amt;
  const value = numericAmt * (10 ** decimals)
  return bigNumberify(Math.floor(value))
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
 * @description  Format currency by network or token
 * @param amt  the amount in the {@link atomicUnit} of the network or token.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 * @param splitValue  where to split the numeric value.
 *   Trailing zeros will be omitted. Excess decimal places will be truncated (not rounded).
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network or token.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 * @example  formatCurrency(bigNumberify('9999998799987000')); // => '9999998799.987'
 */
function handleFormat(amt: unknown, decimals: number, splitValue: number = 6): string {
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

/**
 * @description  Format currency by network
 */
export function formatCurrency(amt: unknown, decimals: number = 6): string {
  return handleFormat(amt, decimals, 6)
}

/**
 * @description  Format currency based on token decimals
 */
export function formatWithDecimals(amt: unknown, decimals: number): string {
  return handleFormat(amt, decimals, decimals)
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
export const getNetworkSecs = async (): Promise<BigNumber> =>
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
  ApplicationID: number,
  Deployer: Address,
};

export const verifyContract = async (info: ContractInfo, bin: Backend): Promise<VerifyResult> => {
  return verifyContract_('', info, bin, newEventQueue());
}

const verifyContract_ = async (label:string, info: ContractInfo, bin: Backend, eq: EventQueue): Promise<VerifyResult> => {
  must_be_supported(bin);
  // @ts-ignore
  const ai_bn: BigNumber = protect(T_Contract, info);
  const ApplicationID: number = bigNumberToNumber(ai_bn);
  const { appApproval, appClear, mapDataKeys, stateKeys } =
    bin._Connectors.ALGO;

  let dhead = `${label}: verifyContract`;

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`${dhead} failed: ${msg}`);
    }
  };
  const chkeq = (a: unknown, e:unknown, msg:string) => {
    const as = JSON.stringify(a);
    const es = JSON.stringify(e);
    chk(as === es, `${msg}: expected ${es}, got ${as}`);
  };

  const appInfoM = await getApplicationInfoM(ApplicationID);
  if ( 'exn' in appInfoM ) {
    throw Error(`${dhead} failed: failed to lookup application (${ApplicationID}): ${JSON.stringify(appInfoM.exn)}`);
  }
  const appInfo = appInfoM.val;
  const appInfo_p = appInfo['params'];
  debug(dhead, {appInfo_p});
  chk(appInfo_p !== undefined, `Cannot lookup ApplicationId`);
  chkeq(appInfo_p['approval-program'], appApproval, `Approval program does not match Reach backend`);
  chkeq(appInfo_p['clear-state-program'], appClear, `ClearState program does not match Reach backend`);
  const Deployer = appInfo_p['creator'];

  const appInfo_LocalState = appInfo_p['local-state-schema'];
  chkeq(appInfo_LocalState['num-byte-slice'], appLocalStateNumBytes + mapDataKeys, `Num of byte-slices in local state schema does not match Reach backend`);
  chkeq(appInfo_LocalState['num-uint'], appLocalStateNumUInt, `Num of uints in local state schema does not match Reach backend`);

  const appInfo_GlobalState = appInfo_p['global-state-schema'];
  chkeq(appInfo_GlobalState['num-byte-slice'], appGlobalStateNumBytes + stateKeys, `Num of byte-slices in global state schema does not match Reach backend`);
  chkeq(appInfo_GlobalState['num-uint'], appGlobalStateNumUInt, `Num of uints in global state schema does not match Reach backend`);

  chkeq(appInfo['deleted'] === true, false, `Application must not be deleted`);
  eq.init({ ApplicationID });

  // Next, we check that it was created with this program and wasn't created
  // with a different program first (which could have modified the state)
  const iat = await eq.deq(dhead);
  debug({iat});
  chkeq(iat['created-application-index'], ApplicationID, 'app created');
  chkeq(iat['application-index'], 0, 'app created');
  const allocRound = appInfo['created-at-round'];
  if ( allocRound ) {
    chkeq(iat['confirmed-round'], allocRound, 'created on correct round');
  }
  chkeq(iat['approval-program'], appInfo_p['approval-program'], `ApprovalProgram unchanged since creation`);
  chkeq(iat['clear-state-program'], appInfo_p['clear-state-program'], `ClearStateProgram unchanged since creation`);

  return { ApplicationID, Deployer };
};

/**
 * Formats an account's address in the way users expect to see it.
 * @param acc Account, NetworkAccount, base32-encoded address, or hex-encoded address
 * @returns the address formatted as a base32-encoded string with checksum
 */
export function formatAddress(acc: string|NetworkAccount|Account): string {
  return addressFromHex(T_Address.canonicalize(acc));
}

export function unsafeGetMnemonic(acc: NetworkAccount|Account): string {
  // @ts-ignore
  const networkAccount: NetworkAccount = acc.networkAccount || acc;
  if (!networkAccount.sk) { throw Error(`unsafeGetMnemonic: Secret key not accessible for account`); }
  return algosdk.secretKeyToMnemonic(networkAccount.sk);
}

export async function launchToken (accCreator:Account, name:string, sym:string, opts:LaunchTokenOpts = {}) {
  debug(`Launching token, ${name} (${sym})`);
  const addr = (acc:Account) => acc.networkAccount.addr;
  const caddr = addr(accCreator);
  const zaddr = caddr;
  // ^ XXX should be nothing; docs say can be "", but doesn't actually work
  const client = await getAlgodClient();
  const dotxn = async (mktxn:(params:TxnParams) => Transaction, acc:Account = accCreator) => {
    const sk = acc.networkAccount.sk;
    if ( ! sk ) {
      throw new Error(`can only launchToken with account with secret key`);
    }
    const params = await getTxnParams('launchToken');
    const t = mktxn(params);
    const s = t.signTxn(sk);
    const r = (await client.sendRawTransaction(s).do());
    return await waitForConfirmation(r.txId);
  };
  const supply = opts.supply ? bigNumberify(opts.supply) : bigNumberify(2).pow(64).sub(1);
  const decimals = opts.decimals !== undefined ? opts.decimals : 6;
  const ctxn_p = await dotxn(
    (params:TxnParams) =>
    algosdk.makeAssetCreateTxnWithSuggestedParams(
      caddr, undefined, bigNumberToBigInt(supply), decimals,
      false, zaddr, zaddr, zaddr, zaddr,
      sym, name, '', '', params,
    ));
  const idn = ctxn_p['created-asset-index'];
  if ( ! idn ) { throw Error(`${sym} no asset-index!`); }
  const id = bigNumberify(idn);
  debug(`${sym}: asset is ${id}`);

  const mint = async (accTo:Account, amt:unknown) => {
    debug(`${sym}: transferring ${amt} ${sym} for ${addr(accTo)}`);
    await transfer(accCreator, accTo, amt, id);
  };
  const optOut = async (accFrom:Account, accTo:Account = accCreator) => {
    await dotxn(
      (params) =>
      algosdk.makeAssetTransferTxnWithSuggestedParams(
        addr(accFrom), addr(accTo), addr(accTo), undefined,
        0, undefined, idn, params
      ), accFrom);
  };
  return { name, sym, id, mint, optOut };
};

export const reachStdlib = stdlib;
