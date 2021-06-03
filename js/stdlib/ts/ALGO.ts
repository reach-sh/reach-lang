export const connector = 'ALGO';

// XXX: use @types/algosdk when we can
import algosdk from 'algosdk';
import base32 from 'hi-base32';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import buffer from 'buffer';
import * as msgpack from '@msgpack/msgpack';

// DEBUG: uncomment this for debugging in browser
// @ts-ignore
// import algosdk__src__transaction from 'algosdk/src/transaction';

const {Buffer} = buffer;

import {
  VERSION
} from './version';
import {
  CurrencyAmount, OnProgress,
  IBackend, IBackendViewInfo, IBackendViewsInfo, getViewsHelper,
  IAccount, IContract, IRecv, ISimRes, ISimTxn,
  deferContract,
  debug, envDefault,
  argsSlice, argsSplit,
  makeRandom,
  replaceableThunk,
} from './shared_impl';
import {
  mapRef,
} from './shared_backend';
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
  addressToHex,
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
  sign: (txn: TXN) => Promise<{blob: string /* base64 */, txID: string}>,
  accounts: (args: {ledger: string}) => Promise<Array<{address: string}>>,
}
// TODO: find the proper algo terminology for Wallet
// XXX this doesn't quite match whatever algosdk thinks a Wallet is,
// because this also includes AlgoSigner
type Wallet = {
    addr: Address,
    sk?: SecretKey,
    AlgoSigner?: AlgoSigner,
  }
type SignedTxn = Uint8Array;
type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
    fee: number,
    group?: any,
    signTxn: (sk: SecretKey) => SignedTxn,
  }
type TxnParams = {
  flatFee: boolean,
  fee: number,
  firstRound: number,
  lastRound: number,
  genesisID: number,
  genesisHash: string,
}
type StatusInfo = {
    'last-round': number,
  }
type TxIdWrapper = {
    toString: () => string
  }
type TxnInfo = {
    'confirmed-round': number,
    'application-index'?: number,
  }
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

type StepArgInfo = {
  count: number,
  size: number,
};

const reachAlgoBackendVersion = 1;
type Backend = IBackend<AnyALGO_Ty> & {_Connectors: {ALGO: {
  version: number,
  appApproval0: string,
  appApproval: string,
  appClear: string,
  ctc: string,
  viewSize: number,
  viewKeys: number,
  mapDataSize: number,
  mapDataKeys: number,
  mapRecordSize: number,
  mapArgSize: number,
  steps: Array<string|null>,
  stepargs: Array<StepArgInfo|null>,
  unsupported: Array<string>,
}}};
type BackendViewsInfo = IBackendViewsInfo<AnyALGO_Ty>;
type BackendViewInfo = IBackendViewInfo<AnyALGO_Ty>;

type CompiledBackend = {
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
  ctc: CompileResultBytes,
  steps: Array<CompileResultBytes|null>,
};

type ContractInfo = {
  getInfo?: () => Promise<ContractInfo>,
  creationRound: number,
  ApplicationID: number,
  Deployer: Address,
};

type Digest = BigNumber
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Digest, Address, Token, AnyALGO_Ty>;
type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo>
type SimRes = ISimRes<Digest, Token, AnyALGO_Ty>
type SimTxn = ISimTxn<Token>

// Helpers

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

type SignStrategy = 'mnemonic' | 'AlgoSigner' | 'MyAlgo';

const [getSignStrategy, setSignStrategy] = replaceableThunk<SignStrategy>(() => 'mnemonic');
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
  const algodClient = await getAlgodClient();
  let lastRound: null | number = null;
  do {
    const lastRoundAfterCall: ApiCall<StatusInfo> = lastRound ?
      algodClient.statusAfterBlock(lastRound) :
      algodClient.status();
    lastRound = (await lastRoundAfterCall.do())['last-round'];
    const pendingInfo =
      await algodClient.pendingTransactionInformation(txId).do();
    const confirmedRound = pendingInfo['confirmed-round'];
    if (confirmedRound && confirmedRound > 0) {
      return pendingInfo;
    }
  } while (!untilRound || lastRound < untilRound);

  throw { type: 'waitForConfirmation', txId, untilRound, lastRound };
};

type STX = {
  lastRound: number,
  txID: string,
  tx: SignedTxn, // Uint8Array
}

const sendAndConfirm = async (
  stx_or_stxs: STX | Array<STX>,
): Promise<TxnInfo> => {
  // @ts-ignore
  let {lastRound, txID, tx} = stx_or_stxs;
  let sendme = tx;
  if (Array.isArray(stx_or_stxs)) {
    if (stx_or_stxs.length === 0) {
      // debug(`Sending nothing... why...?`);
      // @ts-ignore
      return null;
    }
    // debug(`Sending multiple...`);
    lastRound = stx_or_stxs[0].lastRound;
    txID = stx_or_stxs[0].txID;
    sendme = stx_or_stxs.map((stx) => stx.tx);
  }
  const untilRound = lastRound;
  const req = (await getAlgodClient()).sendRawTransaction(sendme);
  // @ts-ignore
  debug('sendAndConfirm:', base64ify(req.txnBytesToPost));
  try {
    await req.do();
  } catch (e) {
    throw { type: 'sendRawTransaction', e };
  }
  return await waitForConfirmation(txID, untilRound);
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
  while (true) {
    const params = await (await getAlgodClient()).getTransactionParams().do();
    debug('fillTxn: got params:', params);
    if (params.firstRound !== 0) {
      return params;
    }
    debug(`...but firstRound is 0, so let's wait and try again.`);
    // Assumption: firstRound will move past 0 on its own.
    await Timeout.set(1000);
  }
};

// XXX
type TXN = any;

function regroup(thisAcc: NetworkAccount, txns: Array<Txn>): Array<TXN> {
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
  txn: Txn,
): Promise<TxnInfo> => {
  const txn_s = await signTxn(networkAccount, txn);
  try {
    return await sendAndConfirm(txn_s);
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

const replaceUint8Array = (label: string, arr: Uint8Array, x:string): string =>
  replaceAll(x, `"{{${label}}}"`, `base32(${base32.encode(arr).toString()})`);

const replaceAddr = (label: string, addr: Address, x:string): string =>
  replaceUint8Array(label, algosdk.decodeAddress(addr).publicKey, x);

function must_be_supported(bin: Backend) {
  const algob = bin._Connectors.ALGO;
  const { unsupported, version } = algob;
  if ( version !== reachAlgoBackendVersion ) {
    throw Error(`This Reach compiled backend does not match the expectations of this Reach standard library: expected ${reachAlgoBackendVersion}, but got ${version}; update your compiler and recompile!`);
  }
  if ( unsupported.length > 0 ) {
    const reasons = unsupported.map(s => ` * ${s}`).join('\n');
    throw Error(`This Reach application is not supported on Algorand for the following reasons:\n${reasons}`);
  }
}

// Get these from stdlib
const MaxTxnLife = 1000;
const LogicSigMaxSize = 1000;
const MaxAppArgs = 16;
const MaxAppTotalArgLen = 2048;
const MaxAppProgramLen = 1024;
const MaxAppTxnAccounts = 4;
const HowManyAccounts = MaxAppTxnAccounts + 1;

async function compileFor(bin: Backend, info: ContractInfo): Promise<CompiledBackend> {
  const { ApplicationID, Deployer } = info;
  must_be_supported(bin);
  const algob = bin._Connectors.ALGO;

  const { appApproval, appClear, ctc, steps, stepargs } = algob;

  const subst_appid = (x: string) =>
    replaceUint8Array(
      'ApplicationID',
      T_UInt.toNet(bigNumberify(ApplicationID)),
      x);
  const subst_creator = (x: string) =>
    replaceAddr('Deployer', Deployer, x);

  const checkLen = (label:string, actual:number, expected:number): void => {
    debug(`checkLen`, {label, actual, expected});
    if ( actual > expected ) {
        throw Error(`This Reach application is not supported by Algorand: ${label} length is ${actual}, but should be less than ${expected}.`); } };

  const ctc_bin = await compileTEAL('ctc_subst', subst_creator(subst_appid(ctc)));
  checkLen(`Escrow Contract`, ctc_bin.result.length, LogicSigMaxSize);
  const subst_ctc = (x: string) =>
    replaceAddr('ContractAddr', ctc_bin.hash, x);

  let appApproval_subst = appApproval;
  const stepCode_bin: Array<CompileResultBytes|null> =
    await Promise.all(steps.map(async (mc, mi) => {
      if ( !mc ) { return null; }
      const mN = `m${mi}`;
      const mc_subst = subst_creator(subst_ctc(subst_appid(mc)));
      const cr = await compileTEAL(mN, mc_subst);
      checkLen(`${mN} Contract`, cr.result.length, LogicSigMaxSize);
      const sa = stepargs[mi];
      if ( sa ) {
        checkLen(`${mN} Contract Arguments Count`, sa.count, MaxAppArgs);
        checkLen(`${mN} Contract Arguments Length`, sa.size, MaxAppTotalArgLen);
      }
      appApproval_subst =
        replaceAddr(mN, cr.hash, appApproval_subst);
      return cr;
  }));

  const appApproval_bin =
    await compileTEAL('appApproval_subst', appApproval_subst);
  checkLen(`Approval Contract`, appApproval_bin.result.length, MaxAppProgramLen);
  const appClear_bin =
    await compileTEAL('appClear', appClear);
  checkLen(`Clear Contract`, appClear_bin.result.length, MaxAppProgramLen);

  return { appApproval: appApproval_bin,
    appClear: appClear_bin,
    ctc: ctc_bin,
    steps: stepCode_bin,
  };
}

// const ui8z = new Uint8Array();

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

const doQuery_ = async (dhead:string, query: ApiCall<any>): Promise<any> => {
  debug(dhead, '--- QUERY =', query);
  let res;
  try {
    res = await query.do();
  } catch (e) {
    throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
  }
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

  if ( ptxns.length == 0 ) {
    return { succ: false, round: res['current-round'] };
  }

  const txn = ptxns[0];
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

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
export const [getAlgodClient, setAlgodClient] = replaceableThunk(async () => {
  debug(`Setting algod client to default`);
  return await waitAlgodClientFromEnv(envDefaultsALGO(process.env));
});

export const [getIndexer, setIndexer] = replaceableThunk(async () => {
  debug(`setting indexer to default`);
  return await waitIndexerFromEnv(envDefaultsALGO(process.env));
});

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
const rawFaucetDefaultMnemonic = 'husband sock drift razor piece february loop nose crew object salon come sketch frost grocery capital young strategy catalog dial seminar sword betray absent army';
const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  const ledger = getLedger();
  if (ledger !== localhostProviderEnv.ALGO_LEDGER) {
    throw Error(`Cannot automatically use faucet for ledger '${ledger}'; if you want to use a custom faucet, use setFaucet`);
  }
  const FAUCET = algosdk.mnemonicToSecretKey(
    envDefault(process.env.ALGO_FAUCET_PASSPHRASE, rawFaucetDefaultMnemonic),
  );
  return await connectAccount(FAUCET);
});

export {getFaucet, setFaucet};

// XXX AlgoSigner doesn't correctly handle msgpacked notes
// When it does: update {,un}clean_for_AlgoSigner
// const note = algosdk.encodeObj('Reach');
const NOTE_Reach = new Uint8Array(Buffer.from(`Reach ${VERSION}`));

const makeTransferTxn = (
  from: Address,
  to: Address,
  value: BigNumber,
  token: Token|undefined,
  ps: TxnParams,
  closeTo: Address|undefined = undefined,
): Txn => {
  const valuen = bigNumberToNumber(value);
  const txn =
    token ?
      algosdk.makeAssetTransferTxnWithSuggestedParams(
        from, to, closeTo, undefined,
        valuen, NOTE_Reach, bigNumberToNumber(token), ps)
    :
      algosdk.makePaymentTxnWithSuggestedParams(
        from, to, valuen, closeTo, NOTE_Reach, ps);
  return txn;
};

export const transfer = async (
  from: Account,
  to: Account,
  value: any,
  token: Token|undefined = undefined,
): Promise<TxnInfo> => {
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;
  const valuebn = bigNumberify(value);
  const ps = await getTxnParams();
  const txn = makeTransferTxn(sender.addr, receiver, valuebn, token, ps);

  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuebn}`,
    sender,
    txn);
};

async function signTxn(networkAccount: NetworkAccount, txnOrig: Txn | any): Promise<STX> {
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
    const { Deployer, ApplicationID } = ctcInfo;
    let lastRound = ctcInfo.creationRound;
    debug(shad, ': attach', ApplicationID, 'created at', lastRound);

    const bin_comp = await compileFor(bin, ctcInfo);
    const escrowAddr = bin_comp.ctc.hash;
    void(addressToHex);
    // XXX const escrowAddrRaw = T_Address.canonicalize(addressToHex(escrowAddr));
    await verifyContract(ctcInfo, bin);
    const ctc_prog = algosdk.makeLogicSig(bin_comp.ctc.result, []);

    const { viewSize, viewKeys, mapDataKeys } = bin._Connectors.ALGO;
    const hasMaps = mapDataKeys > 0;
    const { mapDataTy } = bin._getMaps({reachStdlib: compiledStdlib});
    const mapRecordTy =
      T_Tuple([ T_Bool, mapDataTy, mapDataTy, T_Address ]);
    const mapArgTy =
      T_Array(mapRecordTy, HowManyAccounts);
    const emptyMapDataTy = T_Bytes(mapDataTy.netSize);
    const emptyMapData =
      // This is a bunch of Nones
      mapDataTy.fromNet(
        emptyMapDataTy.toNet(emptyMapDataTy.canonicalize('')));
    debug({ emptyMapData });

    // Application Local State Opt-in
    const didOptIn = async (): Promise<boolean> => {
      const client = await getAlgodClient();
      const ai = await client.accountInformation(thisAcc.addr).do();
      debug(`didOptIn`, ai);
      if ( ai['apps-local-state'].find((x:any) => (x.id === ApplicationID)) ) {
        return true;
      } else {
        return false;
      }
    };
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
      return await waitUntilTime(bigNumberify(lastRound).add(delta));
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
      if ( hasLastTime !== false ) {
        const ltidx = hasLastTime.toNumber();
        tys.splice(ltidx, 1);
        args.splice(ltidx, 1);
      }
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

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const [ svs, msg ] = argsSplit(args, evt_cnt);
      const [ svs_tys, msg_tys ] = argsSplit(tys, evt_cnt);
      const fake_res = {
        didTimeout: false,
        data: msg,
        time: bigNumberify(0), // This should not be read.
        value: value,
        from: pks,
        getOutput: (async (o_lab:string, o_ctc:any): Promise<any> => {
          void(o_lab);
          void(o_ctc);
          throw Error(`Algorand does not support remote calls, and Reach should not have generated a call to this function`);
        }),
      };
      const sim_r = await sim_p( fake_res );
      debug(dhead , '--- SIMULATE', sim_r);
      const { isHalt } = sim_r;
      const sim_txns = sim_r.txns;

      // Views
      const [ view_ty, view_v ] = sim_r.view;
      debug(dhead, 'VIEW', { view_ty, view_v });
      const view_tysz = view_ty.netSize;
      const padding = Math.max(viewSize - view_tysz, 0);
      const padding_ty = T_Bytes(padding);
      const padding_v = padding_ty.canonicalize('');
      const [ view_typ, view_vp ] =
        viewSize > 0 ?
        [ T_Tuple([view_ty, padding_ty]), [ view_v,  padding_v] ]:
        [ padding_ty, padding_v ];
      debug(dhead, 'VIEWP', { view_typ, view_vp });

      // Parse CBR into Public Key
      const cbr2algo_addr = (x:string): Address =>
        algosdk.encodeAddress(Buffer.from(x.slice(2), 'hex'));

      // Maps
      const { mapRefs, mapsPrev, mapsNext } = sim_r;
      type MapRecord = [ boolean, any, any, any ];
      const mapAccts: Array<Address> = [ ];
      const mapArg: Array<MapRecord> = [ ];
      const emptyRec = (caddr:any): MapRecord =>
        [ false, emptyMapData, emptyMapData, caddr ];
      const getMapData = (maps:any, addr:any) =>
        maps.map((m: any) => mapRef(m, addr));
      const mkMapRecord = (isSender:boolean) => (addr:Address) => {
        const caddr = T_Address.canonicalize(addr);
        const addrIdx =
          mapArg.findIndex((mr:MapRecord) => addressEq(mr[3], caddr));
        const present = addrIdx !== -1;
        if (present) { return; }
        const refIdx =
          mapRefs.findIndex((other:Address) => addressEq(other, caddr));
        const used = refIdx !== -1;
        const record = (rec:MapRecord) => {
          mapArg.push(rec);
          if ( ! isSender ) {
            mapAccts.push(addr);
          }
        };
        if ( used ) {
          record([ true, getMapData(mapsPrev, caddr), getMapData(mapsNext, caddr), caddr ]);
        } else if ( isSender ) {
          record(emptyRec(caddr));
        }
      };
      mkMapRecord(true)(thisAcc.addr);
      mapRefs.map(cbr2algo_addr).forEach(mkMapRecord(false));
      const missingAccts = (HowManyAccounts - mapArg.length);
      const zero_caddr = T_Address.canonicalize('0x00');
      for ( let i = 0; i < missingAccts; i++ ) {
        mapArg.push(emptyRec(zero_caddr));
      }
      debug(dhead, 'MAP', { mapArg, mapArgTy, mapAccts });
      debug(dhead, 'MAPARG', mapArg );
      if ( hasMaps ) {
        await ensureOptIn();
      }
      const mapAcctsReal =
        (mapAccts.length === 0) ? undefined : mapAccts;

      while ( true ) {
        const params = await getTxnParams();
        if ( timeout_delay ) {
          const tdn = Math.min(MaxTxnLife, timeout_delay.toNumber());
          params.lastRound = lastRound + tdn;
          debug(dhead, '--- TIMECHECK', { params, timeout_delay, tdn });
          if ( params.firstRound > params.lastRound ) {
            debug(dhead, '--- FAIL/TIMEOUT');
            return {didTimeout: true};
          }
        }

        debug(dhead, '--- ASSEMBLE w/', params);

        let txnToContract_value_idx: number = -1;
        let totalFromFee: number = 0;
        const txnExtraTxns =
          sim_txns.map(
            (t: SimTxn, i: number): Txn => {
              const { tok } = t;
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
                from = escrowAddr;
                to = escrowAddr;
                totalFromFee += raw_minimumBalance;
                amt = t.amt;
              } else if ( t.kind === 'halt' ) {
                from = escrowAddr;
                to = Deployer;
                closeTo = Deployer;
              } else if ( t.kind === 'to' ) {
                from = thisAcc.addr;
                to = escrowAddr;
                amt = t.amt;
              } else {
                assert(false, 'sim txn kind');
              }
              const txn = makeTransferTxn(from, to, amt, tok, params, closeTo);
              if ( from === escrowAddr ) {
                totalFromFee += txn.fee;
              }
              if ( t.kind === 'to' && ! tok ) {
                txnToContract_value_idx = i;
              }
              return txn;
        } );
        debug(dhead, '--- totalFromFee =', totalFromFee);
        assert(txnToContract_value_idx !== -1, 'sim txn no value');
        txnExtraTxns[txnToContract_value_idx] =
          makeTransferTxn(thisAcc.addr, escrowAddr,
                          value.add(totalFromFee),
                          undefined, params);

      const actual_args =
        [ sim_r.prevSt_noPrevTime, sim_r.nextSt_noTime, view_vp, isHalt, bigNumberify(totalFromFee), lastRound, svs, msg, mapArg ];
      const actual_tys =
        [ T_Digest, T_Digest, view_typ, T_Bool, T_UInt, T_UInt, T_Tuple(svs_tys), T_Tuple(msg_tys), mapArgTy ];
      debug(dhead, '--- ARGS =', actual_args);

      const safe_args: Array<NV> = actual_args.map(
        // @ts-ignore
        (m, i) => actual_tys[i].toNet(m));
      safe_args.forEach((x) => {
        if (! ( x instanceof Uint8Array ) ) {
          // The types say this is impossible now,
          // but we'll leave it in for a while just in case...
          throw Error(`expect safe program argument, got ${JSON.stringify(x)}`);
        }
      });
      const ui8h = (x:Uint8Array): string => Buffer.from(x).toString('hex');
      debug(dhead, '--- PREPARE:', safe_args.map(ui8h));

      const handler_sig = algosdk.makeLogicSig(handler.result, []);
      debug(dhead, '--- PREPARED');

        const whichAppl =
          isHalt ?
          // We are treating it like any party can delete the application, but the docs say it may only be possible for the creator. The code appears to not care: https://github.com/algorand/go-algorand/blob/0e9cc6b0c2ddc43c3cfa751d61c1321d8707c0da/ledger/apply/application.go#L589
          algosdk.makeApplicationDeleteTxn :
          algosdk.makeApplicationNoOpTxn;
        const txnAppl =
          whichAppl(
            thisAcc.addr, params, ApplicationID, safe_args,
            mapAcctsReal, undefined, undefined, NOTE_Reach);
        const txnFromHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            handler.hash,
            thisAcc.addr,
            0,
            thisAcc.addr,
            NOTE_Reach,
            params);
        debug(dhead, '--- txnFromHandler =', txnFromHandler);
        const txnToHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            handler.hash,
            txnFromHandler.fee + raw_minimumBalance,
            undefined, NOTE_Reach,
            params);
        debug(dhead, '--- txnToHandler =', txnToHandler);
        const txns = [
          txnAppl,
          txnToHandler,
          txnFromHandler,
          ...txnExtraTxns ];
        algosdk.assignGroupID(txns);
        regroup(thisAcc, txns);

        const signLSTO = (txn: Txn, ls: any): STX => {
          const tx_obj = algosdk.signLogicSigTransactionObject(txn, ls);
          return {
            tx: tx_obj.blob,
            txID: tx_obj.txID,
            lastRound: txn.lastRound,
          };
        };
        const sign_me = async (x: Txn): Promise<STX> => await signTxn(thisAcc, x);

        const txnAppl_s = await sign_me(txnAppl);
        const txnFromHandler_s = signLSTO(txnFromHandler, handler_sig);
        const txnToHandler_s = await sign_me(txnToHandler);
        const txnExtraTxns_s =
          await Promise.all(
            txnExtraTxns.map(
              async (t: Txn, i:number): Promise<STX> => {
                const st = sim_txns[i];
                debug('txnExtraTxns_s', {t, i, st});
                const t_s =
                  st.kind === 'to' ?
                    await sign_me(t)
                  : signLSTO(t, ctc_prog);
                return t_s;
        }));

        const txns_s = [
          txnAppl_s,
          txnToHandler_s,
          txnFromHandler_s,
          ...txnExtraTxns_s,
        ];

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
      // Ignoring this, because no ALGO dev node
      void(waitIfNotPresent);
      const indexer = await getIndexer();

      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} recv ${funcName} ${timeout_delay}`;
      debug(dhead, '--- START');

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const timeoutRound =
        timeout_delay ?
        lastRound + timeout_delay.toNumber() :
        undefined;

      while ( true ) {
        let hquery = indexer.searchForTransactions()
          .address(handler.hash)
          .addressRole('sender')
          // Look at the next one after the last message
          // XXX when we implement firstMsg, this won't work on the first
          // message
          .minRound(lastRound + 1);
        if ( timeoutRound ) {
          hquery = hquery.maxRound(timeoutRound);
        }

        const hres = await doQuery(dhead, hquery);
        if ( ! hres.succ ) {
          const currentRound = hres.round;
          if ( timeoutRound && timeoutRound < currentRound ) {
            debug(dhead, '--- RECVD timeout', {timeoutRound, currentRound});
            return { didTimeout: true };
          }

          // XXX perhaps wait until a new round has happened using wait
          await Timeout.set(2000);
          continue;
        }
        const htxn = hres.txn;
        debug(dhead, '--- htxn =', htxn);

        const theRound = htxn['confirmed-round'];
        let query = indexer.searchForTransactions()
          .applicationID(ApplicationID)
          .txType('appl')
          .round(theRound);
        const res =
          // XXX move predicate into indexer query
          await doQuery(dhead, query, ((x:any) => x.group === htxn.group));
        if ( ! res.succ ) {
          // XXX This is probably really bad
          continue;
        }
        const {txn} = res;
        debug(dhead, '--- txn =', txn);

        const ctc_args_all: Array<string> =
          txn['application-transaction']['application-args'];
        debug(dhead, {ctc_args_all});
        const argMsg = 7; // from ALGO.hs
        const ctc_args_s: string = ctc_args_all[argMsg];

        /** @description base64->hex->arrayify */
        const reNetify = (x: string): NV => {
          const s: string = Buffer.from(x, 'base64').toString('hex');
          debug(dhead, '--- reNetify(', x, ') = ', s);
          return ethers.utils.arrayify('0x' + s);
        };

        debug(dhead, '--- tys =', tys);
        const msgTy = T_Tuple(tys);
        const ctc_args = msgTy.fromNet(reNetify(ctc_args_s));
        debug(dhead, {ctc_args});

        const args_un = argsSlice(ctc_args, evt_cnt);
        debug(dhead, '--- args_un =', args_un);

        const argFeeAmount = 3; // from ALGO.hs
        const totalFromFee =
          T_UInt.fromNet(reNetify(ctc_args_all[argFeeAmount]));
        debug(dhead, '--- totalFromFee =', totalFromFee);

        const fromAddr =
          htxn['payment-transaction'].receiver;
        const from =
          T_Address.canonicalize({addr: fromAddr});
        debug(dhead, '--- from =', from, '=', fromAddr);

        const oldLastRound = lastRound;
        lastRound = theRound;
        debug(dhead, '--- RECVD updating round from', oldLastRound, 'to', lastRound);

        const getOutput = (o_lab:String, o_ctc:any): Promise<any> => {
          void(o_lab);
          void(o_ctc);
          throw Error(`Algorand does not support remote calls`);
        };

        return {
          didTimeout: false,
          data: args_un,
          time: bigNumberify(lastRound),
          from, getOutput,
        };
      }
    };

    const creationTime = async () =>
      bigNumberify((await getInfo()).creationRound);

    const views_bin = bin._getViews({reachStdlib: compiledStdlib});
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
        const vvn = new Uint8Array(viewSize);
        let offset = 0;
        for ( let i = 0; i < viewKeys; i++ ) {
          debug({i});
          const ik = base64ify(`v${i}`);
          debug({ik});
          const viewSt = (appSt.find((x:any) => x.key === ik)).value;
          debug({viewSt});
          const vvni = base64ToUI8A(viewSt.bytes);
          debug({vvni});
          if ( vvni.length == 0 ) {
            return ['None', null];
          }
          vvn.set(vvni, offset);
          offset += vvni.length;
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
          const vres = decode(vi, vvs.slice(1), args);
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

    const { appApproval0, appClear, viewKeys, mapDataKeys } = algob;
    const Deployer = thisAcc.addr;

    const appApproval0_subst =
      replaceAddr('Deployer', Deployer, appApproval0);
    const appApproval0_bin =
      await compileTEAL('appApproval0', appApproval0_subst);
    const appClear_bin =
      await compileTEAL('appClear', appClear);

    const createRes =
      await sign_and_send_sync(
        'ApplicationCreate',
        thisAcc,
        algosdk.makeApplicationCreateTxn(
          thisAcc.addr, await getTxnParams(),
          algosdk.OnApplicationComplete.NoOpOC,
          appApproval0_bin.result,
          appClear_bin.result,
          0, mapDataKeys, 2, 1 + viewKeys,
          undefined, undefined, undefined, undefined,
          NOTE_Reach));

    const ApplicationID = createRes['application-index'];
    if ( ! ApplicationID ) {
      throw Error(`No application-index in ${JSON.stringify(createRes)}`);
    }
    const bin_comp = await compileFor(bin, { ApplicationID, Deployer, creationRound: 0 });
    const escrowAddr = bin_comp.ctc.hash;

    const params = await getTxnParams();
    const txnUpdate =
      algosdk.makeApplicationUpdateTxn(
        thisAcc.addr, params,
        ApplicationID, bin_comp.appApproval.result,
        appClear_bin.result,
        undefined, undefined, undefined, undefined,
        NOTE_Reach);
    const txnToContract =
      algosdk.makePaymentTxnWithSuggestedParams(
        thisAcc.addr,
        escrowAddr,
        raw_minimumBalance,
        undefined, NOTE_Reach,
        params);

    const txns = [
      txnUpdate,
      txnToContract,
    ];
    algosdk.assignGroupID(txns);
    regroup(thisAcc, txns);

    const txnUpdate_s =
      await signTxn(thisAcc, txnUpdate);
    const txnToContract_s =
      await signTxn(thisAcc, txnToContract);

    const txns_s = [
      txnUpdate_s,
      txnToContract_s,
    ];

    let updateRes;
    try {
        updateRes = await sendAndConfirm( txns_s );
    } catch (e) {
      throw Error(`deploy: ${JSON.stringify(e)}`);
    }

    const creationRound = updateRes['confirmed-round'];
    const getInfo = async (): Promise<ContractInfo> =>
      ({ ApplicationID, creationRound, Deployer });

    debug(shad, 'application created');
    return await attachP(bin, getInfo());
  };

  const implNow = { stdlib: compiledStdlib };

  const attach = (bin: Backend, ctcInfoP: Promise<ContractInfo>): Contract => {
    return deferContract(false, attachP(bin, ctcInfoP), implNow);
  };

  const deploy = (bin: Backend): Contract => {
    return deferContract(false, deployP(bin), implNow);
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  }

  return { deploy, attach, networkAccount, getAddress: selfAddress, stdlib: compiledStdlib, setDebugLabel };
};

export const balanceOf = async (acc: Account): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  const client = await getAlgodClient();
  const {amount} = await client.accountInformation(networkAccount.addr).do();
  return bigNumberify(amount);
};


export const createAccount = async (): Promise<Account> => {
  const networkAccount = algosdk.generateAccount();
  return await connectAccount(networkAccount);
};

export const fundFromFaucet = async (account: Account, value: any) => {
  const faucet = await getFaucet();
  await transfer(faucet, account, value);
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
  const numericAmt =
    isBigNumber(amt) ? amt.toNumber()
    : typeof amt === 'string' ? parseFloat(amt)
    : amt;
  return bigNumberify(algosdk.algosToMicroalgos(numericAmt));
}

// XXX get from SDK
const raw_minimumBalance = 100000;
export const minimumBalance: BigNumber =
  bigNumberify(raw_minimumBalance);

/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 */
export function formatCurrency(amt: any, decimals: number = 6): string {
  // Recall that 1 algo = 10^6 microalgos
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Use decimals+1 and then slice it off to truncate instead of round
  const algosStr = algosdk
    .microalgosToAlgos(bigNumberify(amt).toNumber())
    .toFixed(decimals+1);
  // Have to roundtrip thru Number to drop trailing zeroes
  return Number(algosStr.slice(0, algosStr.length - 1)).toString();
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
  let currentTime = await getNetworkTime();
  while (currentTime.lt(targetTime)) {
    debug('waitUntilTime: iteration:', currentTime, '->', targetTime);
    const status = await (await getAlgodClient()).statusAfterBlock(currentTime.toNumber()).do();
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

export const verifyContract = async (info: ContractInfo, bin: Backend): Promise<true> => {
  const { ApplicationID, Deployer, creationRound } = info;
  const compiled = await compileFor(bin, info);
  const { appApproval, appClear } = compiled;

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

  const client = await getAlgodClient();
  const appInfo = await client.getApplicationByID(ApplicationID).do();
  const appInfo_p = appInfo['params'];
  debug(dhead, '-- appInfo_p =', appInfo_p);
  const indexer = await getIndexer();
  const cquery = indexer.searchForTransactions()
    .applicationID(ApplicationID)
    .txType('appl')
    .round(creationRound);
  let ctxn = null;
  while ( ! ctxn ) {
    const cres = await doQuery(dhead, cquery);
    if ( ! cres.succ ) {
      if ( cres.round < creationRound ) {
        debug(dhead, '-- waiting for creationRound');
        await Timeout.set(1000);
        continue;
      } else {
        chk(false, `Not created in stated round`);
        break;
      }
    } else {
      ctxn = cres.txn;
      break;
    }
  }
  debug(dhead, '-- ctxn =', ctxn);
  const fmtp = (x: CompileResultBytes) => uint8ArrayToStr(x.result, 'base64');

  chk(ctxn, `Cannot query for creationRound accuracy`);
  chk(appInfo_p, `Cannot lookup ApplicationId`);
  chkeq(appInfo_p['approval-program'], fmtp(appApproval), `Approval program does not match Reach backend`);
  chkeq(appInfo_p['clear-state-program'], fmtp(appClear), `ClearState program does not match Reach backend`);
  chkeq(appInfo_p['creator'], Deployer, `Deployer does not match contract information`);

  const catxn = ctxn['application-transaction'];
  chkeq(catxn['approval-program'], appInfo_p['approval-program'], `creationRound Approval program`);
  chkeq(catxn['clear-state-program'], appInfo_p['clear-state-program'], `creationRound ClearState program`);
  chkeq(catxn['on-completion'], 'update', `creationRound on-completion`);
  chkeq(ctxn['sender'], Deployer, `creationRound Deployer`);

  // Note: (after deployMode:firstMsg is implemented)
  // 1. (above) attach initial args to ContractInfo
  // 2. verify contract storage matches expectations based on initial args

  return true;
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
