export const connector = 'ALGO';

// XXX: use @types/algosdk when we can
import algosdk from 'algosdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import buffer from 'buffer';
import * as msgpack from '@msgpack/msgpack';

import type { Transaction } from 'algosdk';

// DEBUG: uncomment this for debugging in browser
// @ts-ignore
// import algosdk__src__transaction from 'algosdk/src/transaction';

const {Buffer} = buffer;

import {
  VERSION
} from './version';
import {
  CurrencyAmount, OnProgress,
  IViewLib, IBackend, IBackendViewInfo, IBackendViewsInfo, getViewsHelper,
  IAccount, IContract, IRecv, ISimRes, ISimTxn,
  deferContract,
  debug, envDefault,
  argsSlice, argsSplit,
  makeRandom,
  replaceableThunk,
  ensureConnectorAvailable,
  bigNumberToBigInt,
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
  PayAmt,
  ALGO_Ty,
  NV,
  addressFromHex,
  stdlib as compiledStdlib,
  typeDefs,
} from './ALGO_compiled';
import { process, window } from './shim';
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = compiledStdlib;
export * from './shared_user';

// Type Definitions

type BigNumber = ethers.BigNumber;

type AnyALGO_Ty = ALGO_Ty<CBR_Val>;
// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX Copy/pasted type defs from types/algosdk
// This is so that this module can be exported without our custom types/algosdk
// The unused ones are commented out
type Round = number
type Address = string
// type RawAddress = Uint8Array;
type SecretKey = Uint8Array // length 64
type AlgoSigner = {
  sign: (txn: Transaction) => Promise<{blob: string /* base64 */, txID: string}>,
  accounts: (args: {ledger: string}) => Promise<Array<{address: string}>>,
}

// algosdk.Account = {addr, sk}
// This is slightly different:
// Must have sk w/ optional AlgoSigner
// or AlgoSigner w/ optional sk
type Wallet = {
  addr: Address,
  sk: SecretKey,
  AlgoSigner?: AlgoSigner,
} | {
  addr: Address,
  sk?: SecretKey,
  AlgoSigner: AlgoSigner,
};
type SignedTxn = Uint8Array;
type TxnParams = {
  flatFee?: boolean,
  fee: number,
  firstRound: number,
  lastRound: number,
  genesisID: string,
  genesisHash: string,
}
type TxnInfo = {
  'confirmed-round': number,
  'application-index'?: number,
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

type NetworkAccount = Wallet;

const reachAlgoBackendVersion = 2;
type Backend = IBackend<AnyALGO_Ty> & {_Connectors: {ALGO: {
  version: number,
  appApproval: string,
  appClear: string,
  escrow: string,
  viewSize: number,
  viewKeys: number,
  mapDataSize: number,
  mapDataKeys: number,
  unsupported: Array<string>,
}}};
type BackendViewsInfo = IBackendViewsInfo<AnyALGO_Ty>;
type BackendViewInfo = IBackendViewInfo<AnyALGO_Ty>;

type CompiledBackend = {
  ApplicationID: number,
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
  escrow: CompileResultBytes,
};

type ContractInfo = number;
type Digest = BigNumber
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Digest, Address, Token, AnyALGO_Ty>;
type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
type SimRes = ISimRes<Digest, Token, AnyALGO_Ty>
type SimTxn = ISimTxn<Token>

// Helpers

// Parse CBR into Public Key
const cbr2algo_addr = (x:string): Address =>
  algosdk.encodeAddress(Buffer.from(x.slice(2), 'hex'));

function uint8ArrayToStr(a: Uint8Array, enc: 'utf8' | 'base64' = 'utf8') {
  if (!(a instanceof Uint8Array)) {
    console.log(a);
    throw Error(`Expected Uint8Array, got ${a}`);
  }
  return Buffer.from(a).toString(enc);
}

const [getWaitPort, setWaitPort] = replaceableThunk(() => true);
export { setWaitPort };
async function wait1port(server: string, port: string | number) {
  if (!getWaitPort()) return;
  return await waitPort(server, port);
};

// type SignStrategy = 'mnemonic' | 'AlgoSigner' | 'MyAlgo';

const [getSignStrategy, setSignStrategy] = replaceableThunk<string>(() => 'mnemonic');
export { getSignStrategy, setSignStrategy };

const [getAlgoSigner, setAlgoSigner] = replaceableThunk<Promise<AlgoSigner>>(async () => {
  if (window.AlgoSigner) {
    const AlgoSigner = window.AlgoSigner;
    await AlgoSigner.connect();
    return AlgoSigner;
  } else {
    // TODO: wait for a few seconds and try again before giving up
    throw Error(`Can't find AlgoSigner. Please refresh the page and try again.`);
  }
});
export { setAlgoSigner };

if (process.env.REACH_CONNECTOR_MODE == 'ALGO-browser'
  // Yes, this is dumb. TODO something better
  || process.env.REACH_CONNECTOR_MODE === 'ETH-browser') {
  setWaitPort(false);
}

const rawDefaultToken = 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const rawDefaultItoken = 'reach-devnet';

const getLastRound = async (): Promise<Round> =>
  (await (await getAlgodClient()).status().do())['last-round'];

export const waitForConfirmation = async (txId: TxId, untilRound: number|undefined): Promise<TxnInfo> => {
  const doOrDie = async (p: Promise<any>): Promise<any> => {
    try { return await p; }
    catch (e) { return { 'exn': e }; }
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

  const checkAlgod = async (lastLastRound:number): Promise<TxnInfo> => {
    const lastRound = await checkTooLate(lastLastRound);
    const info =
      await doOrDie(client.pendingTransactionInformation(txId).do());
    debug(...dhead, 'info', info);
    if ( info['exn'] ) {
      debug(...dhead, 'switching to indexer on error');
      return await checkIndexer(lastRound);
    } else if ( info['confirmed-round'] > 0 ) {
      debug(...dhead, 'confirmed');
      return info;
    } else if ( info['pool-error'] === '' ) {
      debug(...dhead, 'still in pool, trying again');
      return await checkAlgod(lastRound);
    } else {
      throw Error(`waitForConfirmation: error confirming: ${JSON.stringify(info)}`);
    }
  };

  const checkIndexer = async (lastLastRound: number): Promise<TxnInfo> => {
    const lastRound = await checkTooLate(lastLastRound);
    const indexer = await getIndexer();
    const q = indexer.lookupTransactionByID(txId);
    const res = await doOrDie(doQuery_(JSON.stringify(dhead), q));
    debug(...dhead, 'indexer', res);
    if ( res['exn'] ) {
      debug(...dhead, 'indexer failed, trying again');
      return await checkIndexer(lastRound);
    } else {
      return res['transaction'];
    }
  };

  return await checkAlgod(0);
};

type STX = {
  lastRound: number,
  txID: string,
  tx: SignedTxn, // Uint8Array
};

const sendAndConfirm = async (
  stxs: Array<STX>,
): Promise<TxnInfo> => {
  const { lastRound, txID } = stxs[0];
  const sendme = stxs.map((stx) => stx.tx);
  try {
    const client = await getAlgodClient();
    const req = client.sendRawTransaction(sendme);
    // @ts-ignore
    debug('sendAndConfirm:', base64ify(req.txnBytesToPost));
    await req.do();
  } catch (e) {
    throw { type: 'sendRawTransaction', e };
  }
  try {
    return await waitForConfirmation(txID, lastRound);
  } catch (e) {
    throw { type: 'waitForConfirmation', e };
  }
};


// Backend
const compileTEAL = async (label: string, code: string): Promise<CompileResultBytes> => {
  debug('compile', label);
  let s, r;
  try {
    r = await (await getAlgodClient()).compile(code).do();
    s = 200;
  } catch (e) {
    s = typeof e === 'object' ? e.statusCode : 'not object';
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

export const getTxnParams = async (): Promise<TxnParams> => {
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

function regroup(thisAcc: NetworkAccount, txns: Array<Transaction>): Array<Transaction> {
  // Sorry this is so dumb.
  // Basically, if these go thru AlgoSigner,
  // it will mangle them,
  //  so we need to recalculate the group hash.
  if (thisAcc.AlgoSigner) {
    const roundtrip_txns = txns
      .map(x => clean_for_AlgoSigner(x))
      .map(x => unclean_for_AlgoSigner(x));

    // console.log(`deployP: group`);
    // console.log(txns[0].group);
    // console.log(Buffer.from(txns[0].group, 'base64').toString('base64'));
    // console.log({...txns[0]});

    algosdk.assignGroupID(roundtrip_txns);

    // console.log(`deploy: roundtrip group`);
    // console.log(Buffer.from(roundtrip_txns[0].group, 'base64').toString('base64'));

    const group = roundtrip_txns[0].group;
    // The same thing, but more paranoid:
    // const group = Buffer.from(roundtrip_txns[0].group, 'base64').toString('base64');
    for (const txn of txns) {
      txn.group = group;
    }
    // console.log({...txns[0]});

    return roundtrip_txns;
  } else {
    return txns;
  }
}

// A copy/paste of some logic from AlgoSigner
// packages/extension/src/background/messaging/task.ts
function unclean_for_AlgoSigner(txnOrig: any) {
  const txn = {...txnOrig};
  Object.keys({...txnOrig}).forEach(key => {
    if (txn[key] === undefined || txn[key] === null){
        delete txn[key];
    }
  });

  // Modify base64 encoded fields
  if ('note' in txn && txn.note !== undefined) {
      txn.note = new Uint8Array(Buffer.from(txn.note));
  }
  // Application transactions only
  if (txn && txn.type === 'appl'){
    if ('appApprovalProgram' in txn){
      txn.appApprovalProgram = base64ToUI8A(txn.appApprovalProgram);
    }
    if ('appClearProgram' in txn){
      txn.appClearProgram = base64ToUI8A(txn.appClearProgram);
    }
    if ('appArgs' in txn){
      var tempArgs: Array<Uint8Array> = [];
      txn.appArgs.forEach((element: string) => {
          tempArgs.push(base64ToUI8A(element));
      });
      txn.appArgs = tempArgs;
    }
  }

  // Note: this part is not copy/pasted from AlgoSigner,
  // and isn't even strictly necessary,
  // but it is nice for getting the same signatures from algosdk & AlgoSigner
  if ('group' in txn) {
    txn.group = base64ToUI8A(txn.group);
  }
  return txn;
}

const clean_for_AlgoSigner = (txnOrig: any) => {
  // Make a copy with just the properties, because reasons
  const txn = {...txnOrig};

  // AlgoSigner does weird things with fees if you don't specify flatFee
  txn.flatFee = true;

  // "Creation of PaymentTx has extra or invalid fields: name,tag,appArgs."
  delete txn.name;
  delete txn.tag;

  // uncaught (in promise) lease must be a Uint8Array.
  // it is... but how about we just delete it instead
  // This is presumed safe when lease is empty
  if (txn.lease instanceof Uint8Array && txn.lease.length === 0) {
    delete txn.lease;
  } else {
    console.log(txn.lease);
    throw Error(`Impossible: non-empty lease`);
  }

  // Creation of ApplTx has extra or invalid fields: nonParticipation
  if (!txn.nonParticipation) {
    delete txn.nonParticipation;
  } else {
    throw Error(`Impossible: expected falsy nonParticipation, got: ${txn.nonParticipation}`);
  }

  // "Creation of ApplTx has extra or invalid fields: name,tag."
  if (txn.type !== 'appl') {
    delete txn.appArgs;
  } else {
    if (txn.appArgs) {
      if (txn.appArgs.length === 0) {
        txn.appArgs = [];
      } else {
        txn.appArgs = txn.appArgs.map((arg: Uint8Array) => uint8ArrayToStr(arg, 'base64'));
      }
    }
  }

  // Validation failed for transaction because of invalid properties [from,to]
  // closeRemainderTo can cause an error w/ js-algorand-sdk addr parsing
  for (const field of ['from', 'to', 'closeRemainderTo']) {
    if (txn[field] && txn[field].publicKey) {
      txn[field] = algosdk.encodeAddress(txn[field].publicKey);
    }
  }

  // Weirdly, AlgoSigner *requires* the note to be a string
  // note is the only field that needs to be utf8-encoded, so far...
  for (const field of ['note']) {
    if (txn[field] && typeof txn[field] !== 'string') {
      txn[field] = uint8ArrayToStr(txn[field], 'utf8');
    }
  }

  // Uncaught (in promise) First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.
  // No idea what it's talking about, but probably GenesisHash?
  // And some more uint8Array BS
  for (const field of ['genesisHash', 'appApprovalProgram', 'appClearProgram', 'group']) {
    if (txn[field] && typeof txn[field] !== 'string') {
      txn[field] = uint8ArrayToStr(txn[field], 'base64');
    }
  }

  return txn;
};

const sign_and_send_sync = async (
  label: string,
  networkAccount: NetworkAccount,
  txn: Transaction,
): Promise<TxnInfo> => {
  const txn_s = await signTxn(networkAccount, txn);
  try {
    return await sendAndConfirm([txn_s]);
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
  const { unsupported, version } = algob;
  if ( version !== reachAlgoBackendVersion ) {
    const older = (version === undefined) || (version < reachAlgoBackendVersion);
    const more = older ? `update your compiler and recompile!` : `updated your standard library and rerun!`;
    throw Error(`This Reach compiled backend does not match the expectations of this Reach standard library: expected ${reachAlgoBackendVersion}, but got ${version}; ${more}`);
  }
  if ( unsupported.length > 0 ) {
    const reasons = unsupported.map(s => ` * ${s}`).join('\n');
    throw Error(`This Reach application is not supported on Algorand for the following reasons:\n${reasons}`);
  }
}

// Get these from stdlib
const MaxTxnLife = 1000;
const LogicSigMaxSize = 1000;
const MaxAppProgramLen = 2048;
const MaxAppTxnAccounts = 4;
const MaxExtraAppProgramPages = 3;

async function compileFor(bin: Backend, info: ContractInfo): Promise<CompiledBackend> {
  if ( ! Number.isInteger(info) ) {
    throw Error(`This Reach standard library cannot communicate with this contract, because it was deployed with an earlier version of Reach.`); }
  const ApplicationID = info;
  must_be_supported(bin);
  const algob = bin._Connectors.ALGO;
  const { appApproval, appClear, escrow } = algob;

  const subst_appid = (x: string) =>
    replaceAll(x, '{{ApplicationID}}', `${ApplicationID}`);

  const checkLen = (label:string, actual:number, expected:number): void => {
    debug(`checkLen`, {label, actual, expected});
    if ( actual > expected ) {
        throw Error(`This Reach application is not supported by Algorand: ${label} length is ${actual}, but should be less than ${expected}.`); } };

  const appApproval_bin =
    await compileTEAL('appApproval_subst', appApproval);
  const appClear_bin =
    await compileTEAL('appClear', appClear);
  checkLen(`App Program Length`, (appClear_bin.result.length + appApproval_bin.result.length), (1 + MaxExtraAppProgramPages) * MaxAppProgramLen);
  const escrow_bin =
    await compileTEAL('escrow_subst', subst_appid(escrow));
  checkLen(`Escrow Contract`, escrow_bin.result.length, LogicSigMaxSize);

  return {
    ApplicationID,
    appApproval: appApproval_bin,
    appClear: appClear_bin,
    escrow: escrow_bin,
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

const doQuery_ = async <T>(dhead:string, query: ApiCall<T>, alwaysRetry: boolean = false): Promise<T> => {
  debug(dhead, '--- QUERY =', query);
  let retries = 10;
  let res;
  while ( retries > 0 ) {
    try {
      res = await query.do();
      break;
    } catch (e) {
      if ( e?.errno === -111 || e?.code === "ECONNRESET" || e?.response?.text === "{\"message\":\"accounting not initialized\"}\n" ) {
        debug(dhead, 'NO CONNECTION');
      } else if ( ! alwaysRetry || retries <= 0 ) {
        throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
      }
      debug(dhead, 'RETRYING', retries--, {e});
      await Timeout.set(500);
    }
  }
  if (!res) { throw Error(`impossible: query res is empty`); }
  debug(dhead, '--- RESULT =', res);
  return res;
};

type QueryResult =
  | { succ: true, txn: any }
  | { succ: false, round: number }

const doQuery = async (dhead:string, query: ApiCall<any>, pred: ((x:any) => boolean) = ((x) => { void(x); return true; })): Promise<QueryResult> => {
  const res = await doQuery_(dhead, query);
  const txns = res.transactions;
  const ptxns = txns.filter(pred);
  debug(dhead, {ptxns});

  if ( ptxns.length == 0 ) {
    return { succ: false, round: res['current-round'] };
  }

  const txn = ptxns.reduce((accum: any, x: any) =>
    (x['confirmed-round'] < accum['confirmed-round']) ? x : accum, ptxns[0]);
  return { succ: true, txn };
};

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

export const { addressEq, tokenEq, digest } = compiledStdlib;

export const { T_Null, T_Bool, T_UInt, T_Tuple, T_Array, T_Object, T_Data, T_Bytes, T_Address, T_Digest, T_Struct, T_Token } = typeDefs;

export const { randomUInt, hasRandom } = makeRandom(8);

export const [getLedger, setLedger] = replaceableThunk<string|undefined>(() => DEFAULT_ALGO_LEDGER);
function getLedgerFromAlgoSigner(AlgoSigner: AlgoSigner) {
  // XXX: get AlgoSigner to tell us what Ledger is "currently selected"
  // since that ability doesn't actually exist, we operate based off of setLedger()
  void(AlgoSigner);
  return getLedger();
}

async function waitIndexerFromEnv(env: ProviderEnv): Promise<algosdk.Indexer> {
  const { ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT, ALGO_INDEXER_TOKEN } = env;
  await wait1port(ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);
  return new algosdk.Indexer(ALGO_INDEXER_TOKEN, ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);
}

async function waitAlgodClientFromEnv(env: ProviderEnv): Promise<algosdk.Algodv2> {
  const { ALGO_SERVER, ALGO_PORT, ALGO_TOKEN } = env;
  await wait1port(ALGO_SERVER, ALGO_PORT);
  return new algosdk.Algodv2(ALGO_TOKEN, ALGO_SERVER, ALGO_PORT);
}

// TODO: read token from scripts/devnet-algo/algorand_data/algod.token
export const [getAlgodClient, setAlgodClient] = replaceableThunk(async () => {
  debug(`Setting algod client to default`);
  return await waitAlgodClientFromEnv(envDefaultsALGO(process.env));
});

export const [getIndexer, setIndexer] = replaceableThunk(async () => {
  debug(`setting indexer to default`);
  return await waitIndexerFromEnv(envDefaultsALGO(process.env));
});

// This function should be provided by the indexer, but it isn't so we simulate
// something decent. This function is allowed to "fail" by not really waiting
// until the round
const indexer_statusAfterBlock = async (round: number): Promise<void> => {
  const client = await getAlgodClient();
  await client.statusAfterBlock(round);
  // XXX Don't move on to next step if not actually this round
  // const indexer = await getIndexer();
  // XXX Wait until the indexer has seen it, but using health check
  await Timeout.set(500);
};

interface ALGO_Provider {
  algodClient: algosdk.Algodv2,
  indexer: algosdk.Indexer,
  ledger?: string
}

export async function getProvider(): Promise<ALGO_Provider> {
  return {
    algodClient: await getAlgodClient(),
    indexer: await getIndexer(),
    ledger: getLedger(),
  }
}
export async function setProvider(provider: ALGO_Provider|Promise<ALGO_Provider>): Promise<void> {
  provider = await provider;
  // XXX doesn't waitPort these, because these are opaque to us.
  // should we do something similar where we wait for /health to give us a 200 response?
  setAlgodClient((async () => provider.algodClient)());
  setIndexer((async () => provider.indexer)());
  setLedger(provider.ledger);
}

export interface ProviderEnv {
  // ALGO_LEDGER may be undefined under some circumstances,
  // but AlgoSigner codepaths will error if it is undefined.
  ALGO_LEDGER: string|undefined
  ALGO_SERVER: string
  ALGO_PORT: string
  ALGO_TOKEN: string
  ALGO_INDEXER_SERVER: string
  ALGO_INDEXER_PORT: string
  ALGO_INDEXER_TOKEN: string
}

const localhostProviderEnv: ProviderEnv = {
  ALGO_LEDGER: 'Reach Devnet',
  ALGO_SERVER: 'http://localhost',
  ALGO_PORT: '4180',
  ALGO_TOKEN: rawDefaultToken,
  ALGO_INDEXER_SERVER: 'http://localhost',
  ALGO_INDEXER_PORT: '8980',
  ALGO_INDEXER_TOKEN: rawDefaultItoken,
}

const DEFAULT_ALGO_LEDGER = localhostProviderEnv.ALGO_LEDGER;
const DEFAULT_ALGO_SERVER = localhostProviderEnv.ALGO_SERVER;
const DEFAULT_ALGO_PORT = localhostProviderEnv.ALGO_PORT;
const DEFAULT_ALGO_TOKEN = localhostProviderEnv.ALGO_TOKEN;
const DEFAULT_ALGO_INDEXER_SERVER = localhostProviderEnv.ALGO_INDEXER_SERVER;
const DEFAULT_ALGO_INDEXER_PORT = localhostProviderEnv.ALGO_INDEXER_PORT;
const DEFAULT_ALGO_INDEXER_TOKEN = localhostProviderEnv.ALGO_INDEXER_TOKEN;

function serverLooksLikeRandlabs(server: string): boolean {
  return server.toLowerCase().includes('algoexplorerapi.io');
}

function envDefaultALGOPort(port: string|undefined, defaultPort: string, server: string): string {
  // Some simple guessing
  return port !== undefined ? port
    : serverLooksLikeRandlabs(server) ? ''
    : defaultPort;
}

function envDefaultALGOToken(token: string|undefined, defaultToken: string, server: string, port: string): string {
  // Some simple guessing
  // port is not currently used for this guessing, but could be in the future
  void(port);
  return token !== undefined ? token
    : serverLooksLikeRandlabs(server) ? ''
    : defaultToken;
}

function guessRandlabsLedger(server?: string): string|undefined {
  if (server === undefined) return undefined;
  server = server.toLowerCase();
  if (server.startsWith('https://algoexplorerapi.io')) {
    return 'MainNet';
  } else if (server.startsWith('https://testnet.algoexplorerapi.io')) {
    return 'TestNet';
  } else if (server.startsWith('https://betanet.algoexplorerapi.io')) {
    return 'BetaNet';
  }
  return undefined;
}

function envDefaultALGOLedger(ledger: string|undefined, defaultLedger: string|undefined, server: string, port: string): string|undefined {
  // Some simple guessing
  // port is not currently used for this guessing, but could be in the future
  void(port);
  return ledger !== undefined ? ledger
    : serverLooksLikeRandlabs(server) ? guessRandlabsLedger(ledger)
    : defaultLedger;
}

function envDefaultsALGO(env: Partial<ProviderEnv>): ProviderEnv {
  const ALGO_SERVER = envDefault(env.ALGO_SERVER, DEFAULT_ALGO_SERVER);
  const ALGO_PORT = envDefaultALGOPort(env.ALGO_PORT, DEFAULT_ALGO_PORT, ALGO_SERVER);
  const ALGO_TOKEN = envDefaultALGOToken(env.ALGO_TOKEN, DEFAULT_ALGO_TOKEN, ALGO_SERVER, ALGO_PORT);
  const ALGO_LEDGER = envDefaultALGOLedger(env.ALGO_LEDGER, DEFAULT_ALGO_LEDGER, ALGO_SERVER, ALGO_PORT);

  const ALGO_INDEXER_SERVER = envDefault(env.ALGO_INDEXER_SERVER, DEFAULT_ALGO_INDEXER_SERVER);
  const ALGO_INDEXER_PORT = envDefaultALGOPort(env.ALGO_INDEXER_PORT, DEFAULT_ALGO_INDEXER_PORT, ALGO_INDEXER_SERVER);
  const ALGO_INDEXER_TOKEN = envDefaultALGOToken(env.ALGO_INDEXER_TOKEN, DEFAULT_ALGO_INDEXER_TOKEN, ALGO_INDEXER_SERVER, ALGO_INDEXER_PORT);

  return {
    ALGO_LEDGER,
    ALGO_SERVER,
    ALGO_PORT,
    ALGO_TOKEN,
    ALGO_INDEXER_SERVER,
    ALGO_INDEXER_PORT,
    ALGO_INDEXER_TOKEN,
  }
}

export function setProviderByEnv(env: Partial<ProviderEnv>): void {
  // Note: This doesn't just immediately call setProviderByEnv,
  // because here we can actually take the opportunity to wait1port.
  const fullEnv = envDefaultsALGO(env);

  setAlgodClient(waitAlgodClientFromEnv(fullEnv));
  setIndexer(waitIndexerFromEnv(fullEnv));
  setLedger(fullEnv.ALGO_LEDGER);
}

type WhichNetExternal
  = 'MainNet'
  | 'TestNet'
  | 'BetaNet'

export type ProviderName
  = WhichNetExternal
  | 'LocalHost'
  | 'randlabs/MainNet'
  | 'randlabs/TestNet'
  | 'randlabs/BetaNet'

function randlabsProviderEnv(ALGO_LEDGER: WhichNetExternal): ProviderEnv {
  const prefix = ALGO_LEDGER === 'MainNet' ? '' : `${ALGO_LEDGER.toLowerCase()}.`;
  const RANDLABS_BASE = `https://${prefix}algoexplorerapi.io`;
  return {
    ALGO_LEDGER,
    ALGO_SERVER: RANDLABS_BASE,
    ALGO_PORT: '',
    ALGO_TOKEN: '',
    ALGO_INDEXER_SERVER: `${RANDLABS_BASE}/idx2`,
    ALGO_INDEXER_PORT: '',
    ALGO_INDEXER_TOKEN: '',
  }
}

export function providerEnvByName(providerName: ProviderName): ProviderEnv {
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

export function setProviderByName(providerName: ProviderName): void {
  return setProviderByEnv(providerEnvByName(providerName));
}

// eslint-disable-next-line max-len
const rawFaucetDefaultMnemonic = 'around sleep system young lonely length mad decline argue army veteran knee truth sell hover any measure audit page mammal treat conduct marble above shell';
const [getFaucet, setFaucet_] = replaceableThunk(async (): Promise<Account> => {
  if ( ! isIsolatedNetwork() ) {
    throw Error(`Cannot automatically use faucet for non-isolated network; if you want to use a custom faucet, use setFaucet`);
  }
  const FAUCET = algosdk.mnemonicToSecretKey(
    envDefault(process.env.ALGO_FAUCET_PASSPHRASE, rawFaucetDefaultMnemonic),
  );
  return await connectAccount(FAUCET);
});
let settedFaucet = false;
const setFaucet = (x:Promise<Account>) => {
  settedFaucet = true;
  setFaucet_(x);
};
const isIsolatedNetwork = (): boolean =>
  (settedFaucet || getLedger() === localhostProviderEnv.ALGO_LEDGER);
export {getFaucet, setFaucet};

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
): Promise<TxnInfo> => {
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;
  const valuebn = bigNumberify(value);
  const ps = await getTxnParams();
  const txn = makeTransferTxn(sender.addr, receiver, valuebn, token, ps, undefined, tag);

  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuebn}`,
    sender,
    txn);
};

async function signTxn(networkAccount: NetworkAccount, txnOrig: Transaction): Promise<STX> {
  const {sk, AlgoSigner} = networkAccount;
  if (sk && !AlgoSigner) {
    const tx = txnOrig.signTxn(sk);
    const ret = {
      tx,
      txID: txnOrig.txID().toString(),
      lastRound: txnOrig.lastRound,
    };
    debug('signed sk_ret');
    debug({txID: ret.txID});
    debug(msgpack.decode(ret.tx));
    return ret;
  } else if (AlgoSigner) {
    // TODO: clean up txn before signing
    const txn = clean_for_AlgoSigner(txnOrig);

    // Note: don't delete the following,
    // it is extremely useful for debugging when stuff changes wrt AlgoSigner/algosdk clashes

    // if (sk) {
    //   const re_tx = txnOrig.signTxn ? txnOrig : new algosdk__src__transaction.Transaction(txnOrig);
    //   re_tx.group = txnOrig.group;

    //   const sk_tx = re_tx.signTxn(sk);
    //   const sk_ret = {
    //     tx: sk_tx,
    //     txID: re_tx.txID().toString(),
    //     lastRound: txnOrig.lastRound,
    //   };
    //   console.log('signed sk_ret');
    //   console.log({txID: sk_ret.txID});
    //   console.log(msgpack.decode(sk_ret.tx));
    // }

    debug('AlgoSigner.sign ...');
    const stx_obj = await AlgoSigner.sign(txn);
    debug('...signed');
    debug({stx_obj});
    const ret = {
      tx: Buffer.from(stx_obj.blob, 'base64'),
      txID: stx_obj.txID,
      lastRound: txnOrig.lastRound,
    };

    debug('signed AlgoSigner');
    debug({txID: ret.txID});
    debug(msgpack.decode(ret.tx));
    return ret;
  } else {
    throw Error(`networkAccount has neither sk nor AlgoSigner: ${JSON.stringify(networkAccount)}`);
  }
}

const makeIsMethod = (i:number) => (txn:any): boolean =>
  txn['application-transaction']['application-args'][0] === base64ify([i]);

/** @description base64->hex->arrayify */
const reNetify = (x: string): NV => {
  const s: string = Buffer.from(x, 'base64').toString('hex');
  return ethers.utils.arrayify('0x' + s);
};

export const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const thisAcc = networkAccount;
  const shad = thisAcc.addr.substring(2, 6);
  let label = shad;
  const pks = T_Address.canonicalize(thisAcc);
  debug(shad, ': connectAccount');

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

  const attachP = async (bin: Backend, ctcInfoP: Promise<ContractInfo>): Promise<Contract> => {
    const ctcInfo = await ctcInfoP;
    const getInfo = async () => ctcInfo;
    const { compiled, ApplicationID, allocRound, ctorRound, Deployer } =
      await verifyContract(ctcInfo, bin);
    debug(shad, 'attach', {ApplicationID, allocRound, ctorRound} );
    let realLastRound = ctorRound;

    const escrowAddr = compiled.escrow.hash;
    const escrow_prog = algosdk.makeLogicSig(compiled.escrow.result, []);

    const { viewSize, viewKeys, mapDataKeys, mapDataSize } = bin._Connectors.ALGO;
    const hasMaps = mapDataKeys > 0;
    const { mapDataTy } = bin._getMaps({reachStdlib: compiledStdlib});
    const emptyMapDataTy = T_Bytes(mapDataTy.netSize);
    const emptyMapData =
      // This is a bunch of Nones
      mapDataTy.fromNet(
        emptyMapDataTy.toNet(emptyMapDataTy.canonicalize('')));
    debug({ emptyMapData });

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
      await sign_and_send_sync(
        'ApplicationOptIn',
        thisAcc,
        algosdk.makeApplicationOptInTxn(
          thisAcc.addr, await getTxnParams(),
          ApplicationID,
          undefined, undefined, undefined, undefined,
          NOTE_Reach));
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

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      return await waitUntilTime(bigNumberify(realLastRound).add(delta));
    };

    const sendrecv = async (
      funcNum: number,
      evt_cnt: number,
      hasLastTime: (BigNumber | false),
      tys: Array<AnyALGO_Ty>,
      args: Array<any>,
      pay: PayAmt,
      out_tys: Array<AnyALGO_Ty>,
      onlyIf: boolean,
      soloSend: boolean,
      timeout_delay: false | BigNumber,
      sim_p: (fake: Recv) => Promise<SimRes>,
    ): Promise<Recv> => {
      void (hasLastTime);
      const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
        await recv(funcNum, evt_cnt, out_tys, waitIfNotPresent, timeout_delay);
      if ( ! onlyIf ) {
        return await doRecv(true);
      }

      const [ value, toks ] = pay;
      void(toks); // <-- rely on simulation because of ordering

      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} sendrecv ${funcName} ${timeout_delay}`;
      debug(dhead, '--- START');

      const [ svs, msg ] = argsSplit(args, evt_cnt);
      const [ svs_tys, msg_tys ] = argsSplit(tys, evt_cnt);
      const fake_res = {
        didTimeout: false,
        data: msg,
        time: bigNumberify(0), // This should not be read.
        value: value,
        from: pks,
        getOutput: (async (o_mode:string, o_lab:string, o_ctc:any): Promise<any> => {
          void(o_mode);
          void(o_lab);
          void(o_ctc);
          throw Error(`Algorand does not support remote calls, and Reach should not have generated a call to this function`);
        }),
      };
      const sim_r = await sim_p( fake_res );
      debug(dhead , '--- SIMULATE', sim_r);
      const { isHalt } = sim_r;

      // Maps
      const { mapRefs } = sim_r;
      const mapAccts: Array<Address> = [ ];
      mapRefs.forEach((caddr:Address) => {
        const addr = cbr2algo_addr(caddr);
        if ( addressEq(thisAcc.addr, addr) ) { return; }
        const addrIdx =
          mapAccts.findIndex((other:Address) => addressEq(other, addr));
        const present = addrIdx !== -1;
        if ( present ) { return; }
        mapAccts.push(addr);
      });
      if ( mapAccts.length > MaxAppTxnAccounts ) {
        throw Error(`Application references too many local state cells in one step. Reach should catch this problem statically.`);
      }
      debug(dhead, 'MAP', { mapAccts });
      if ( hasMaps ) { await ensureOptIn(); }
      const mapAcctsReal = (mapAccts.length === 0) ? undefined : mapAccts;

      const sign_escrow = async (txn: Transaction): Promise<STX> => {
        const tx_obj = algosdk.signLogicSigTransactionObject(txn, escrow_prog);
        return {
          tx: tx_obj.blob,
          txID: tx_obj.txID,
          lastRound: txn.lastRound,
        };
      };
      const sign_me =
        async (x: Transaction): Promise<STX> => await signTxn(thisAcc, x);

      while ( true ) {
        const params = await getTxnParams();
        if ( timeout_delay ) {
          const tdn = Math.min(MaxTxnLife, timeout_delay.toNumber());
          params.lastRound = realLastRound + tdn;
          debug(dhead, '--- TIMECHECK', { params, timeout_delay, tdn });
          // We add one, because the firstRound field is actually the current
          // round, which we couldn't possibly be in, because it already
          // happened.
          if ( params.firstRound + 1 > params.lastRound ) {
            debug(dhead, '--- FAIL/TIMEOUT');
            return {didTimeout: true};
          }
        }

        debug(dhead, '--- ASSEMBLE w/', params);

        let extraFees: number = 0;
        type Signer = (x: Transaction) => Promise<STX>;
        const txnExtraTxns: Array<Transaction> = [];
        const txnExtraTxns_signers: Array<Signer> = [];
        let sim_i = 0;
        const processSimTxn = (t: SimTxn) => {
          let signer: Signer = sign_escrow;
          let txn;
          if ( t.kind === 'tokenNew' ) {
            processSimTxn({
              kind: 'to',
              amt: minimumBalance,
              tok: undefined,
            });
            const zaddr = undefined;
            const ap = bigNumberToBigInt(t.p);
            debug(`tokenNew`, t.p, ap);
            txn = algosdk.makeAssetCreateTxnWithSuggestedParams(
              escrowAddr, NOTE_Reach_tag(sim_i++), ap, 6,
              false, escrowAddr, zaddr, zaddr, zaddr,
              t.s, t.n, t.u, t.m, params,
            );
          } else if ( t.kind === 'tokenBurn' ) {
            // There's no burning on Algorand
            return;
          } else if ( t.kind === 'tokenDestroy' ) {
            txn = algosdk.makeAssetDestroyTxnWithSuggestedParams(
              escrowAddr, NOTE_Reach_tag(sim_i++),
              bigNumberToNumber(t.tok), params,
            );
            // XXX We could get the minimum balance back after
          } else {
            const { tok } = t;
            let always: boolean = false;
            let amt: BigNumber = bigNumberify(0);
            let from: Address = escrowAddr;
            let to: Address = escrowAddr;
            let closeTo: Address|undefined = undefined;
            if ( t.kind === 'from' ) {
              from = escrowAddr;
              // @ts-ignore
              to = cbr2algo_addr(t.to);
              amt = t.amt;
            } else if ( t.kind === 'init' ) {
              processSimTxn({
                kind: 'to',
                amt: minimumBalance,
                tok: undefined,
              });
              from = escrowAddr;
              to = escrowAddr;
              always = true;
              amt = t.amt;
            } else if ( t.kind === 'halt' ) {
              from = escrowAddr;
              to = Deployer;
              closeTo = Deployer;
              always = true;
            } else if ( t.kind === 'to' ) {
              from = thisAcc.addr;
              to = escrowAddr;
              amt = t.amt;
              signer = sign_me;
            } else {
              assert(false, 'sim txn kind');
            }
            if ( ! always && amt.eq(0) ) { return; }
            txn = makeTransferTxn(from, to, amt, tok, params, closeTo, sim_i++);
          }
          extraFees += txn.fee;
          txn.fee = 0;
          txnExtraTxns.push(txn);
          txnExtraTxns_signers.push(signer);
        };
        sim_r.txns.forEach(processSimTxn);
        debug(dhead, 'txnExtraTxns', txnExtraTxns);
        debug(dhead, '--- extraFee =', extraFees);

      const actual_args = [ svs, msg ];
      const actual_tys = [ T_Tuple(svs_tys), T_Tuple(msg_tys) ];
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
            mapAcctsReal, undefined, undefined, NOTE_Reach);
        txnAppl.fee += extraFees;
        const txns = [ ...txnExtraTxns, txnAppl ];
        algosdk.assignGroupID(txns);
        regroup(thisAcc, txns);

        const txnAppl_s = await sign_me(txnAppl);
        const txnExtraTxns_s =
          await Promise.all(
            txnExtraTxns.map(
              async (t: Transaction, i:number): Promise<STX> =>
                await txnExtraTxns_signers[i](t)
        ));

        const txns_s = [ ...txnExtraTxns_s, txnAppl_s ];
        debug(dhead, '--- SEND:', txns_s.length);
        let res;
        try {
          res = await sendAndConfirm( txns_s );

          // XXX we should inspect res and if we failed because we didn't get picked out of the queue, then we shouldn't error, but should retry and let the timeout logic happen.
          debug(dhead, '--- SUCCESS:', res);
        } catch (e) {
          if ( e.type == 'sendRawTransaction' ) {
            debug(dhead, '--- FAIL:', format_failed_request(e.e));
          } else {
            debug(dhead, '--- FAIL:', e);
          }

          if ( ! soloSend ) {
            // If there is no soloSend, then someone else "won", so let's
            // listen for their message
            return await doRecv(false);
          }

          if ( timeout_delay ) {
            // If there can be a timeout, then keep waiting for it
            continue;
          } else {
            // Otherwise, something bad is happening
            throw Error(`${dhead} --- ABORT`);
          }
        }

        return await doRecv(false);
      }
    };

    const recv = async (
      funcNum: number,
      evt_cnt: number,
      tys: Array<AnyALGO_Ty>,
      waitIfNotPresent: boolean,
      timeout_delay: false | BigNumber,
    ): Promise<Recv> => {
      const indexer = await getIndexer();

      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} recv ${funcName} ${timeout_delay}`;
      debug(dhead, '--- START');

      const timeoutRound =
        timeout_delay ?
        realLastRound + timeout_delay.toNumber() :
        undefined;

      while ( true ) {
        let query = indexer.searchForTransactions()
          .applicationID(ApplicationID)
          .txType('appl')
          // Look at the next one after the last message
          // XXX when we implement firstMsg, this won't work on the first
          // message
          .minRound(realLastRound + 1);
        if ( timeoutRound ) {
          query = query.maxRound(timeoutRound);
        }

        const correctStep = makeIsMethod(funcNum);
        const res = await doQuery(dhead, query, correctStep);
        if ( ! res.succ ) {
          const currentRound = res.round;
          if ( timeoutRound && timeoutRound <= currentRound ) {
            debug(dhead, '--- RECVD timeout', {timeoutRound, currentRound});
            return { didTimeout: true };
          }
          if ( waitIfNotPresent ) {
            await waitUntilTime(bigNumberify(currentRound + 1));
          } else {
            await indexer_statusAfterBlock(currentRound + 1);
          }
          continue;
        }
        const txn = res.txn;
        debug(dhead, '--- txn =', txn);
        const theRound = txn['confirmed-round'];

        let all_txns: Array<any>|undefined = undefined;
        const get_all_txns = async () => {
          if ( all_txns ) { return; }
          const all_query = indexer.searchForTransactions()
            .txType('acfg')
            .assetID(0)
            .round(theRound);
          const all_res = await doQuery_(dhead, all_query);
          // NOTE: Move this filter into the query when the indexer supports it
          const same_group = ((x:any) => x.group === txn.group && x['asset-config-transaction']['asset-id'] === 0);
          const all_txns_raw = all_res.transactions.filter(same_group);
          const group_order = ((x:any, y:any) => x['intra-round-offset'] - y['intra-round-offset']);
          all_txns = all_txns_raw.sort(group_order);
          debug(dhead, 'all_txns', all_txns);
        };

        const ctc_args_all: Array<string> =
          txn['application-transaction']['application-args'];
        debug(dhead, {ctc_args_all});
        const argMsg = 2; // from ALGO.hs
        const ctc_args_s: string = ctc_args_all[argMsg];

        debug(dhead, '--- tys =', tys);
        const msgTy = T_Tuple(tys);
        const ctc_args = msgTy.fromNet(reNetify(ctc_args_s));
        debug(dhead, {ctc_args});

        const args_un = argsSlice(ctc_args, evt_cnt);
        debug(dhead, '--- args_un =', args_un);

        const fromAddr = txn['sender'];
        const from =
          T_Address.canonicalize({addr: fromAddr});
        debug(dhead, '--- from =', from, '=', fromAddr);

        const oldLastRound = realLastRound;
        realLastRound = theRound;
        debug(dhead, '--- RECVD updating round from', oldLastRound, 'to', realLastRound);

        let tokenNews = 0;
        const getOutput = async (o_mode:string, o_lab:string, o_ctc:any): Promise<any> => {
          if ( o_mode === 'tokenNew' ) {
            await get_all_txns();
            // NOTE: I'm making a dangerous assumption that the created tokens
            // are viewed in the order they were created. It would be better to
            // be able to have the JS simulator determine where they are
            // exactly, but it is not available for receives. :'(
            // @ts-ignore
            const tn_txn = all_txns[tokenNews++];
            debug(dhead, "tn_txn", tn_txn);
            return tn_txn['created-asset-index'];
          } else {
            void(o_lab);
            void(o_ctc);
            throw Error(`Algorand does not support remote calls`);
          }
        };

        return {
          didTimeout: false,
          data: args_un,
          time: bigNumberify(realLastRound),
          from, getOutput,
        };
      }
    };

    const creationTime = async () =>
      bigNumberify(ctorRound);

    const recoverSplitBytes = (prefix:string, size:number, howMany:number, src:any): any => {
      const bs = new Uint8Array(size);
      let offset = 0;
      for ( let i = 0; i < howMany; i++ ) {
        debug({prefix, i});
        const ik = base64ify(new Uint8Array([i]));
        debug({ik});
        const st = (src.find((x:any) => x.key === ik)).value;
        debug({st});
        const bsi = base64ToUI8A(st.bytes);
        debug({bsi});
        if ( bsi.length == 0 ) {
          return undefined;
        }
        bs.set(bsi, offset);
        offset += bsi.length;
      }
      return bs;
    };
    const viewlib: IViewLib = {
      viewMapRef: async (mapi: number, a:any): Promise<any> => {
        debug('viewMapRef', { mapi, a });
        const ls = await getLocalState(cbr2algo_addr(a));
        assert(ls !== undefined, 'viewMapRef ls undefined');
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
    const views_bin = bin._getViews({reachStdlib: compiledStdlib}, viewlib);
    const getView1 = (vs:BackendViewsInfo, v:string, k:string, vim: BackendViewInfo) =>
      async (...args: any[]): Promise<any> => {
        debug('getView1', v, k, args);
        const { decode } = vim;
        const client = await getAlgodClient();
        let appInfo;
        try {
          appInfo = await client.getApplicationByID(ApplicationID).do();
        } catch (e) {
          debug('getApplicationById', e);
          return ['None', null];
        }
        const appSt = appInfo['params']['global-state'];
        const vvn = recoverSplitBytes('v', viewSize, viewKeys, appSt);
        if ( vvn === undefined ) {
            return ['None', null];
        }
        const vin = T_UInt.fromNet(vvn.slice(0, T_UInt.netSize));
        const vi = bigNumberToNumber(vin);
        debug({vi});
        const vtys = vs[vi];
        debug({vtys});
        if ( ! vtys ) {
          return ['None', null]; }
        const vty = T_Tuple([T_UInt, ...vtys]);
        debug({vty});
        const vvs = vty.fromNet(vvn);
        debug({vvs});
        try {
          const vres = await decode(vi, vvs.slice(1), args);
          debug({vres});
          return ['Some', vres];
        } catch (e) {
          debug(`getView1`, v, k, 'error', e);
          return ['None', null];
        }
    };
    const getViews = getViewsHelper(views_bin, getView1);

    return { getInfo, creationTime, sendrecv, recv, wait, iam, selfAddress, getViews, stdlib: compiledStdlib };
  };

  const deployP = async (bin: Backend): Promise<Contract> => {
    must_be_supported(bin);
    debug(shad, 'deploy');
    const algob = bin._Connectors.ALGO;
    const { viewKeys, mapDataKeys } = algob;
    const { appApproval, appClear } = await compileFor(bin, 0);
    const extraPages =
      Math.ceil((appClear.result.length + appApproval.result.length) / MaxAppProgramLen) - 1;

    debug(`deploy`, {extraPages});
    const createRes =
      await sign_and_send_sync(
        'ApplicationCreate',
        thisAcc,
        algosdk.makeApplicationCreateTxn(
          thisAcc.addr, await getTxnParams(),
          algosdk.OnApplicationComplete.NoOpOC,
          appApproval.result,
          appClear.result,
          appLocalStateNumUInt, appLocalStateNumBytes + mapDataKeys,
          appGlobalStateNumUInt, appGlobalStateNumBytes + viewKeys,
          undefined, undefined, undefined, undefined,
          NOTE_Reach, undefined, undefined, extraPages));

    const ApplicationID = createRes['application-index'];
    if ( ! ApplicationID ) {
      throw Error(`No application-index in ${JSON.stringify(createRes)}`);
    }
    debug(`created`, {ApplicationID});
    const ctcInfo = ApplicationID;
    const { escrow } = await compileFor(bin, ctcInfo);
    const escrowAddr = escrow.hash;

    debug(`funding escrow`);
    // @ts-ignore
    await transfer({ networkAccount: thisAcc }, { networkAccount: { addr: escrow.hash } }, minimumBalance );
    debug(`call ctor`);
    const params = await getTxnParams();
    const ctor_args =
      [ new Uint8Array([0]),
        T_Address.toNet( T_Address.canonicalize(escrowAddr) ),
        T_Tuple([]).toNet([]) ];
    debug({ctor_args});
    const txnCtor =
      algosdk.makeApplicationNoOpTxn(
        thisAcc.addr, params, ApplicationID, ctor_args,
        undefined, undefined, undefined, NOTE_Reach);
    debug({txnCtor});
    const txnCtor_s = await signTxn(thisAcc, txnCtor);
    try {
      await sendAndConfirm( [ txnCtor_s ] );
    } catch (e) {
      throw Error(`deploy: ${JSON.stringify(e)}`);
    }
    const getInfo = async (): Promise<ContractInfo> => ctcInfo;
    await waitCtorTxn(shad, ctcInfo);
    debug(shad, 'application created');
    return await attachP(bin, getInfo());
  };

  const implNow = { stdlib: compiledStdlib };

  const attach = (bin: Backend, ctcInfoP: Promise<ContractInfo>): Contract => {
    ensureConnectorAvailable(bin._Connectors, connector);
    return deferContract(false, attachP(bin, ctcInfoP), implNow);
  };

  const deploy = (bin: Backend): Contract => {
    ensureConnectorAvailable(bin._Connectors, connector);
    return deferContract(false, deployP(bin), implNow);
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  }
  
  async function tokenAccept(token:Token): Promise<void> {
    debug(`tokenAccept`, token);
    // @ts-ignore
    await transfer(this, this, 0, token);
  };
  const tokenMetadata = async (token:Token): Promise<any> => {
    debug(`tokenMetadata`, token);
    const client = await getAlgodClient();
    const tokenRes = await client.getAssetByID(bigNumberToNumber(token)).do();
    debug({tokenRes});
    const tokenInfo = tokenRes['params'];
    debug({tokenInfo});
    const name = tokenInfo['name'];
    const symbol = tokenInfo['unit-name'];
    const url = tokenInfo['url'];
    const mhr = tokenInfo['metadata-hash'];
    const metadata = mhr ? T_Bytes(32).fromNet(reNetify(mhr)) : undefined;
    const supply = bigNumberify(tokenInfo['total']);
    return { name, symbol, url, metadata, supply };
  };

  return { deploy, attach, networkAccount, getAddress: selfAddress, stdlib: compiledStdlib, setDebugLabel, tokenAccept, tokenMetadata };
};

export const balanceOf = async (acc: Account, token: Token|false = false): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) { 
    throw Error(`acc.networkAccount missing. Got: ${acc}`);
  }
  const client = await getAlgodClient();
  const info = await client.accountInformation(networkAccount.addr).do();
  if ( ! token ) {
    return bigNumberify(info.amount);
  } else {
    for ( const ai of info.assets ) {
      if ( ai['asset-id'] === token ) {
        return ai['amount'];
      }
    }
    return bigNumberify(0);
  }
};


export const createAccount = async (): Promise<Account> => {
  const networkAccount = algosdk.generateAccount();
  return await connectAccount(networkAccount);
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

// XXX get from SDK
const raw_minimumBalance = 100000;
export const minimumBalance: BigNumber =
  bigNumberify(raw_minimumBalance);

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
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 * @example  formatCurrency(bigNumberify('9999998799987000')); // => '9999998799.987'
 */
export function formatCurrency(amt: any, decimals: number = 6): string {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  const amtStr = amt.toString();
  const splitAt = Math.max(amtStr.length - 6, 0);
  const lPredropped = amtStr.slice(0, splitAt);
  const l = ldrop(lPredropped, '0') || '0';
  if (decimals === 0) { return l; }

  const rPre = lpad(amtStr.slice(splitAt), '0', 6);
  const rSliced = rPre.slice(0, decimals);
  const r = rdrop(rSliced, '0');

  return r ? `${l}.${r}` : l;
}

// XXX The getDefaultAccount pattern doesn't really work w/ AlgoSigner
// AlgoSigner does not expose a "currently-selected account"
export async function getDefaultAccount(): Promise<Account> {
  if (!window.prompt) {
    throw Error(`Cannot prompt the user for default account with window.prompt`);
  }
  const signStrategy = getSignStrategy();
  if (signStrategy === 'mnemonic') {
    const mnemonic = window.prompt(`Please paste the mnemonic for your account, or cancel to generate a new one`);
    if (mnemonic) {
      debug(`Creating account from user-provided mnemonic`);
      return await newAccountFromMnemonic(mnemonic);
    } else {
      debug(`No mnemonic provided. Randomly generating a new account secret instead.`);
      return await createAccount();
    }
  } else if (signStrategy === 'AlgoSigner') {
    const AlgoSigner = await getAlgoSigner();
    const ledger = getLedgerFromAlgoSigner(AlgoSigner);
    if (ledger === undefined) throw Error(`Ledger is undefined; this is required by AlgoSigner`);
    const addr = window.prompt(`Please paste your account's address. (This account must be listed in AlgoSigner.)`);
    if (!addr) { throw Error(`No address provided`); }
    return await newAccountFromAlgoSigner(addr, AlgoSigner, ledger);
  } else if (signStrategy === 'MyAlgo') {
    throw Error(`MyAlgo wallet support is not yet implemented`);
  } else {
    throw Error(`signStrategy '${signStrategy}' not recognized. Valid options are 'mnemonic', 'AlgoSigner', and 'MyAlgo'.`);
  }
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
export const newAccountFromSecret = async (secret: string | Uint8Array): Promise<Account> => {
  const sk = ethers.utils.arrayify(secret);
  const mnemonic = algosdk.secretKeyToMnemonic(sk);
  return await newAccountFromMnemonic(mnemonic);
};

export const newAccountFromAlgoSigner = async (addr: string, AlgoSigner: AlgoSigner, ledger: string) => {
  if (!AlgoSigner) {
    throw Error(`AlgoSigner is falsy`);
  }
  const accts = await AlgoSigner.accounts({ledger});
  if (!Array.isArray(accts)) {
    throw Error(`AlgoSigner.accounts('${ledger}') is not an array: ${accts}`);
  }
  if (!accts.map(x => x.address).includes(addr)) {
    throw Error(`Address ${addr} not found in AlgoSigner accounts`);
  }
  let networkAccount = {addr, AlgoSigner};
  return await connectAccount(networkAccount);
};

export const getNetworkTime = async () => bigNumberify(await getLastRound());

export const waitUntilTime = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg = onProgress || (() => {});
  const client = await getAlgodClient();
  let currentTime = await getNetworkTime();
  while (currentTime.lt(targetTime)) {
    debug('waitUntilTime: iteration:', currentTime, '->', targetTime);
    if ( isIsolatedNetwork() ) {
      await fundFromFaucet(await getFaucet(), 0);
    }
    const status = await client.statusAfterBlock(currentTime.toNumber() + 1).do();
    currentTime = bigNumberify(status['last-round']);
    onProg({currentTime, targetTime});
  }
  debug('waitUntilTime: ended:', currentTime, '->', targetTime);
  return currentTime;
};

export const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  debug('wait: delta=', delta, 'now=', now, 'until=', now.add(delta));
  return await waitUntilTime(now.add(delta), onProgress);
};

const appLocalStateNumUInt = 0;
const appLocalStateNumBytes = 0;
const appGlobalStateNumUInt = 0;
const appGlobalStateNumBytes = 1;

type VerifyResult = {
  compiled: CompiledBackend,
  ApplicationID: number,
  allocRound: number,
  ctorRound: number,
  Deployer: Address,
};

export async function queryCtorTxn(dhead: string, ApplicationID: number) {
  const indexer = await getIndexer();
  const icq = indexer.searchForTransactions()
    .applicationID(ApplicationID)
    .txType('appl');
  const isCtor = makeIsMethod(0);
  const icr = await doQuery(`${dhead} ctor`, icq, isCtor);
  debug({icr});
  return icr;
}

async function waitCtorTxn(shad: string, ApplicationID: number): Promise<void> {
  // Note(Dan): Yes, doQuery_ offers retrying, but doQuery has the filtering,
  // and finding the right design point for refactoring is hard,
  // so, I'm just doing some more retrying here.
  // Let's try exponential backoff for a change.
  const maxTries = 14; // SUM(2^n)[1 <= n <= 14] = wait up to 32766 ms
  let icr: any = null;
  for (let tries = 1; tries <= maxTries; tries++) {
    const waitMs = 2 ** tries;
    debug(shad, 'waitCtorTxn waiting (ms)', waitMs);
    await Timeout.set(waitMs);
    debug(shad, 'waitCtorTxn trying attempt #', tries, 'of', maxTries);
    icr = await queryCtorTxn(`${shad} deploy`, ApplicationID);
    if (icr && icr.txn) return;
  }
  throw Error(`Indexer could not find application ${ApplicationID}.`);
}

export const verifyContract = async (info: ContractInfo, bin: Backend): Promise<VerifyResult> => {
  const compiled = await compileFor(bin, info);
  const { ApplicationID, appApproval, appClear } = compiled;
  const { mapDataKeys, viewKeys } = bin._Connectors.ALGO;

  let dhead = `verifyContract`;

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`verifyContract failed: ${msg}`);
    }
  };
  const chkeq = (a: any, e:any, msg:string) => {
    const as = JSON.stringify(a);
    const es = JSON.stringify(e);
    chk(as === es, `${msg}: expected ${es}, got ${as}`);
  };
  const fmtp = (x: CompileResultBytes) => uint8ArrayToStr(x.result, 'base64');

  // XXX it should be okay to wait in this function
  /*
  while ( ! ctxn ) {
    const cres = await doQuery(dhead, cquery);
    if ( ! cres.succ ) {
      if ( cres.round < creationRound ) {
        debug(dhead, `-- waiting for`, {creationRound});
        await indexer_statusAfterBlock(creationRound);
        continue;
      } else {
        chk(false, `Not created in stated round: ${creationRound}`);
      }
    } else {
      ctxn = cres.txn;
    }
  }
  */

  const client = await getAlgodClient();
  const appInfo = await client.getApplicationByID(ApplicationID).do();
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
  chkeq(appInfo_GlobalState['num-byte-slice'], appGlobalStateNumBytes + viewKeys, `Num of byte-slices in global state schema does not match Reach backend`);
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
  const iaq = indexer.searchForTransactions()
    .applicationID(ApplicationID)
    .txType('appl')
    .round(allocRound);
  const iar = await doQuery(`${dhead} alloc`, iaq);
  // @ts-ignore
  const iat = iar.txn;
  chk(iat, `Cannot query for allocation transaction`);
  debug({iat});
  const iatat = iat['application-transaction'];
  debug({iatat});
  chkeq(iatat['approval-program'], appInfo_p['approval-program'], `ApprovalProgram unchanged since creation`);
  chkeq(iatat['clear-state-program'], appInfo_p['clear-state-program'], `ClearStateProgram unchanged since creation`);

  // Next, we check that the constructor was called with the actual escrow
  // address and not something else
  const icr = await queryCtorTxn(dhead, ApplicationID);
  // @ts-ignore
  const ict = icr.txn;
  chk(ict, `Cannot query for constructor transaction`);
  debug({ict});
  const ctorRound = ict['confirmed-round']
  const ictat = ict['application-transaction'];
  debug({ictat});
  const aescrow_b64 = ictat['application-args'][1];
  const aescrow_ui8 = reNetify(aescrow_b64);
  const aescrow_cbr = T_Address.fromNet(aescrow_ui8);
  const aescrow_algo = cbr2algo_addr(aescrow_cbr);
  chkeq( aescrow_algo, compiled.escrow.hash, `Must be constructed with proper escrow account address` );

  // Note: (after deployMode:firstMsg is implemented)
  // 1. (above) attach initial args to ContractInfo
  // 2. verify contract storage matches expectations based on initial args

  return { compiled, ApplicationID, allocRound, ctorRound, Deployer };
};

/**
 * Formats an account's address in the way users expect to see it.
 * @param acc Account, NetworkAccount, base32-encoded address, or hex-encoded address
 * @returns the address formatted as a base32-encoded string with checksum
 */
export function formatAddress(acc: string|NetworkAccount|Account): string {
  return addressFromHex(T_Address.canonicalize(acc));
}

export const reachStdlib = compiledStdlib;
