// ****************************************************************************
// standard library for Javascript users
// ****************************************************************************

// XXX: do not import any types from algosdk; instead copy/paste them below
// XXX: can stop doing this workaround once @types/algosdk is shippable
import algosdk from 'algosdk';
import base32 from 'hi-base32';
import ethers from 'ethers';
import url from 'url';
import Timeout from 'await-timeout';
import buffer from 'buffer';
const {Buffer} = buffer;

import {
  CurrencyAmount, OnProgress, WPArgs,
  debug, getDEBUG,
  isBigNumber, bigNumberify, bigNumberToNumber,
  argsSlice,
  makeRandom,
} from './shared';
import {
  CBR_Address, CBR_Val,
} from './CBR';
import waitPort from 'wait-port';
import { replaceableThunk } from './shared_impl';
import { stdlib as compiledStdlib, ALGO_Ty, NV, typeDefs } from './ALGO_compiled';
import { process, window } from './shim';
export * from './shared';


// ****************************************************************************
// Type Definitions
// ****************************************************************************

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
    sk?: SecretKey, // TODO: describe length? (64)
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
// type AcctInfo = {
//     amount: number // bignumber?
//   }
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
type Account = {
  networkAccount: NetworkAccount,
};

type Backend = {_Connectors: {ALGO: {
  appApproval0: string,
  appApproval: string,
  appClear: string,
  ctc: string,
  steps: Array<string|null>,
  stepargs: Array<number>,
  unsupported: boolean,
}}};

type Digest = BigNumber;
type SimRes = {
  prevSt: Digest,
  prevSt_noPrevTime: Digest,
  txns: Array<SimTxn>,
  nextSt: Digest,
  nextSt_noTime: Digest,
  isHalt : boolean,
};
type SimTxn = {
  to: string,
  amt: BigNumber,
};

type CompiledBackend = {
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
  ctc: CompileResultBytes,
  steps: Array<CompileResultBytes|null>,
};

type Recv = {
  didTimeout: false,
  data: Array<ContractOut>,
  time: BigNumber,
  value: BigNumber,
  from: string,
} | { didTimeout: true };

type ContractAttached = {
  getInfo: () => Promise<ContractInfo>,
  creationTime: () => Promise<BigNumber>,
  sendrecv: (...argz: any) => Promise<Recv>,
  recv: (...argz: any) => Promise<Recv>,
  wait: (...argz: any) => any,
  iam: (some_addr: any) => any,
  selfAddress: () => CBR_Address, // Not RawAddress!
  stdlib: Object,
};

// TODO
type ContractOut = any;

// XXX Add who the creator is to refund them
type ContractInfo = {
  getInfo?: () => Promise<ContractInfo>,
  creationRound: number,
  ApplicationID: number,
};

// ctc[ALGO] = {
//   address: string
//   appId: confirmedTxn.TransactionResults.CreatedAppIndex; // ?
//   creationRound: int // bigint?
//   logic_sig: LogicSig
//
//   // internal fields
//   // * not required to call acc.attach(bin, ctc)
//   // * required by backend
//   sendrecv: function
//   recv: function
// }

// ****************************************************************************
// Helpers
// ****************************************************************************

function uint8ArrayToStr(a: Uint8Array, enc: 'utf8' | 'base64' = 'utf8') {
  return Buffer.from(a).toString(enc);
}

const [getWaitPort, setWaitPort] = replaceableThunk(() => true);
export { setWaitPort };

const [getBrowser, setBrowserRaw] = replaceableThunk(() => false);
const setBrowser = (b: boolean) => {
  if (b) {
    // When in browser, we cannot waitPort
    setWaitPort(false);
  }
  setBrowserRaw(b);
}
export { setBrowser };

// Yes, this is dumb. TODO something better
if (process.env.REACH_CONNECTOR_MODE == 'ETH-test-browser') {
  setBrowser(true);
}

const rawDefaultToken = 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const rawDefaultItoken = 'reach-devnet';

async function wait1port(theServer: string, thePort: string | number) {
  if (!getWaitPort()) return;
  thePort = typeof thePort === 'string' ? parseInt(thePort, 10) : thePort;
  const {hostname} = url.parse(theServer);
  const args: WPArgs = {
    host: hostname || undefined,
    port: thePort,
    output: 'silent',
    timeout: 1000 * 60 * 1,
  }
  debug('wait1port');
  if (getDEBUG()) {
    console.log(args)
  }
  debug('waitPort complete');
  return await waitPort(args);
}

const getLastRound = async (): Promise<Round> =>
  (await (await getAlgodClient()).status().do())['last-round'];

const waitForConfirmation = async (txId: TxId, untilRound: number): Promise<TxnInfo> => {
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
  } while (lastRound < untilRound);

  throw { type: 'waitForConfirmation', txId, untilRound, lastRound };
};

type STX = {
  lastRound: number,
  txID: string,
  tx: SignedTxn, // Uint8Array
}

const sendAndConfirm = async (
  stx_or_stxs: STX | Array<STX>
): Promise<TxnInfo> => {
  // @ts-ignore
  let {lastRound, txID, tx} = stx_or_stxs;
  let sendme = tx;
  if (Array.isArray(stx_or_stxs)) {
    if (stx_or_stxs.length === 0) {
      debug(`Sending nothing... why...?`);
      // @ts-ignore
      return null;
    }
    debug(`Sending multiple...`);
    lastRound = stx_or_stxs[0].lastRound;
    txID = stx_or_stxs[0].txID;
    sendme = stx_or_stxs.map((stx) => stx.tx);
  }
  const untilRound = lastRound;
  const req = (await getAlgodClient()).sendRawTransaction(sendme);
  // @ts-ignore XXX
  debug(`sendAndConfirm: ${base64ify(req.txnBytesToPost)}`);
  try {
    await req.do();
  } catch (e) {
    throw { type: 'sendRawTransaction', e };
  }
  return await waitForConfirmation(txID, untilRound);
};


// Backend
const compileTEAL = async (label: string, code: string): Promise<CompileResultBytes> => {
  debug(`compile ${label}`)
  let s, r;
  try {
    r = await (await getAlgodClient()).compile(code).do();
    s = 200;
  } catch (e) {
    s = typeof e === 'object' ? e.statusCode : 'not object';
    r = e;
  }

  if ( s == 200 ) {
    debug(`compile ${label} succeeded: ${JSON.stringify(r)}`);
    r.src = code;
    r.result = new Uint8Array(Buffer.from(r.result, "base64"));
    // debug(`compile transformed: ${JSON.stringify(r)}`);
    return r;
  } else {
    throw Error(`compile ${label} failed: ${s}: ${JSON.stringify(r)}`);
  }
};

const getTxnParams = async (): Promise<TxnParams> => {
  debug(`fillTxn: getting params`);
  while (true) {
    const params = await (await getAlgodClient()).getTransactionParams().do();
    debug(`fillTxn: got params: ${JSON.stringify(params)}`);
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
    // console.log(Buffer.from(txns[0].group, 'base64').toString('base64'));

    algosdk.assignGroupID(roundtrip_txns);
    // console.log(`deploy: roundtrip group`);
    // console.log(Buffer.from(roundtrip_txns[0].group, 'base64').toString('base64'));

    const group = roundtrip_txns[0].group;
    for (const txn of txns) {
      txn.group = group;
    }
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
    if(txn[key] === undefined || txn[key] === null){
        delete txn[key];
    }
  });

  // Modify base64 encoded fields
  if ('note' in txn && txn.note !== undefined) {
      txn.note = new Uint8Array(Buffer.from(txn.note));
  }
  // Application transactions only
  if(txn && txn.type === 'appl'){
    if('appApprovalProgram' in txn){
      txn.appApprovalProgram = Uint8Array.from(Buffer.from(txn.appApprovalProgram,'base64'));
    }
    if('appClearProgram' in txn){
      txn.appClearProgram = Uint8Array.from(Buffer.from(txn.appClearProgram,'base64'));
    }
    if('appArgs' in txn){
      var tempArgs: Array<Uint8Array> = [];
      txn.appArgs.forEach((element: string) => {
          tempArgs.push(Uint8Array.from(Buffer.from(element,'base64')));
      });
      txn.appArgs = tempArgs;
    }
  }
  return txn;
}

const clean_for_AlgoSigner = (txnOrig: any) => {
  // Make a copy with just the properties, because reasons
  const txn = {...txnOrig};

  // Weirdly, AlgoSigner *requires* the note to be a string
  if (txn.note) {
    txn.note = uint8ArrayToStr(txn.note, 'utf8');
  }
  // Also weirdly:
  // "Creation of PaymentTx has extra or invalid fields: name,tag,appArgs."
  delete txn.name;
  delete txn.tag;
  // "Creation of ApplTx has extra or invalid fields: name,tag."
  if (txn.type !== 'appl') {
    delete txn.appArgs;
  } else {
    if (txn.appArgs) {
      if (txn.appArgs.length === 0) {
        txn.appArgs = [];
      } else {
        // This seems wrong, but it works...
        // XXX The morally correct thing to do would be to msgpack unparse
        txn.appArgs = [uint8ArrayToStr(txn.appArgs, 'base64')];
      }
    }
  }

  // Validation failed for transaction because of invalid properties [from,to]
  if (txn.from && txn.from.publicKey) {
    txn.from = algosdk.encodeAddress(txn.from.publicKey);
  }
  if (txn.to && txn.to.publicKey) {
    txn.to = algosdk.encodeAddress(txn.to.publicKey);
  }
  // Uncaught (in promise) First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.
  // No idea what it's talking about, but probably GenesisHash?
  if (txn.genesisHash) {
    if (typeof txn.genesisHash !== 'string') {
      txn.genesisHash = uint8ArrayToStr(txn.genesisHash, 'base64');
    }
  }
  console.log({genesisHash: txn.genesisHash});
  // uncaught (in promise) lease must be a Uint8Array.
  delete txn.lease; // it is... but how about we just delete it instead

  // Some more uint8Array BS
  if (txn.appApprovalProgram) {
    txn.appApprovalProgram = uint8ArrayToStr(txn.appApprovalProgram, 'base64');
  }
  if (txn.appClearProgram) {
    txn.appClearProgram = uint8ArrayToStr(txn.appClearProgram, 'base64');
  }
  if (txn.group) {
    txn.group = uint8ArrayToStr(txn.group, 'base64');
  }

  // AlgoSigner does weird things with fees if you don't specify flatFee
  txn.flatFee = true;

  return txn;
}

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
  const { unsupported } = algob;
  if ( unsupported ) {
    throw Error(`This Reach application is not supported by Algorand.`);
  }
}

async function compileFor(bin: Backend, ApplicationID: number): Promise<CompiledBackend> {
  must_be_supported(bin);
  const algob = bin._Connectors.ALGO;

  const { appApproval, appClear, ctc, steps, stepargs } = algob;

  const subst_appid = (x: string) =>
    replaceUint8Array(
      'ApplicationID',
      T_UInt.toNet(bigNumberify(ApplicationID)),
      x);

  const ctc_bin = await compileTEAL('ctc_subst', subst_appid(ctc));
  const subst_ctc = (x: string) =>
    replaceAddr('ContractAddr', ctc_bin.hash, x);

  let appApproval_subst = appApproval;
  const stepCode_bin: Array<CompileResultBytes|null> =
    await Promise.all(steps.map(async (mc, mi) => {
      if ( !mc ) { return null; }
      const mN = `m${mi}`;
      const mc_subst = subst_ctc(subst_appid(mc));
      const cr = await compileTEAL(mN, mc_subst);
      const plen = cr.result.length;
      const alen = stepargs[mi];
      const tlen = plen + alen;
      if ( tlen > 1000 ) {
        throw Error(`This Reach application is not supported by Algorand (program(${plen}) + args(${alen}) = total(${tlen}) > 1000)`); }
      appApproval_subst =
        replaceAddr(mN, cr.hash, appApproval_subst);
      return cr;
  }));

  const appApproval_bin =
    await compileTEAL('appApproval_subst', appApproval_subst);
  const appClear_bin =
    await compileTEAL('appClear', appClear);

  return { appApproval: appApproval_bin,
    appClear: appClear_bin,
    ctc: ctc_bin,
    steps: stepCode_bin,
  };
};

const ui8z = new Uint8Array();

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

const doQuery = async (dhead:string, query: ApiCall<any>): Promise<any> => {
  //debug(`${dhead} --- QUERY = ${JSON.stringify(query)}`);
  let res;
  try {
    res = await query.do();
  } catch (e) {
    throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
  }

  if ( res.transactions.length == 0 ) {
    // debug(`${dhead} --- RESULT = empty`);
    // XXX Look at the round in res and wait for a new round
    return null;
  }

  debug(`${dhead} --- RESULT = ${JSON.stringify(res)}`);
  const txn = res.transactions[0];

  return txn;
};

const showBalance = async (note: string, networkAccount: NetworkAccount) => {
  const bal = await balanceOf({ networkAccount });
  const showBal = formatCurrency(bal, 2);
  console.log('%s: balance: %s algos', note, showBal);
};

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

export const { addressEq, digest } = compiledStdlib;

export const { T_Null, T_Bool, T_UInt, T_Tuple, T_Array, T_Object, T_Data, T_Bytes, T_Address, T_Digest } = typeDefs;

export const { randomUInt, hasRandom } = makeRandom(8);

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
const [getAlgodClient, setAlgodClient] = replaceableThunk(async () => {
  debug(`Setting algod client to default`);
  const browser = getBrowser();
  const token = browser
    ? {'X-Algo-API-Token': rawDefaultToken}
    : process.env.ALGO_TOKEN || rawDefaultToken;
  const server = browser
    ? '/algod'
    : process.env.ALGO_SERVER || 'http://localhost';
  const port = browser
    ? ''
    : process.env.ALGO_PORT || '4180';

  if (!browser) await wait1port(server, port);
  return new algosdk.Algodv2(token, server, port);
});

export {setAlgodClient};

const [getIndexer, setIndexer] = replaceableThunk(async () => {
  debug(`setting indexer to default`);
  const browser = getBrowser();
  const itoken = browser
    ? rawDefaultItoken
    : process.env.ALGO_INDEXER_TOKEN || rawDefaultItoken;
  const iserver = browser
    ? '/indexer'
    : process.env.ALGO_INDEXER_SERVER || 'http://localhost';
  const iport = browser
    ? ''
    : process.env.ALGO_INDEXER_PORT || 8980;

  await wait1port(iserver, iport);
  return new algosdk.Indexer(itoken, iserver, iport);
});

export {setIndexer};

// eslint-disable-next-line max-len
const rawFaucetDefaultMnemonic = 'close year slice mind voice cousin brass goat anxiety drink tourist child stock amused rescue pitch exhibit guide occur wide barrel process type able please';
const [getFaucet, setFaucet] = replaceableThunk(async () => {
  const browser = getBrowser();
  const FAUCET = algosdk.mnemonicToSecretKey(browser
    ? rawFaucetDefaultMnemonic
    : process.env.ALGO_FAUCET_PASSPHRASE || rawFaucetDefaultMnemonic
  );
  return await connectAccount(FAUCET);
});

export {getFaucet, setFaucet};


export const transfer = async (from: Account, to: Account, value: any): Promise<TxnInfo> => {
  const valuen = bigNumberToNumber(value);
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;

  const note = algosdk.encodeObj('@reach-sh/ALGO.mjs transfer');
  return await sign_and_send_sync(
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuen}`,
    sender,
    algosdk.makePaymentTxnWithSuggestedParams(
      sender.addr, receiver, valuen, undefined, note, await getTxnParams(),
    ));
};

async function signTxn(networkAccount: NetworkAccount, txnOrig: Txn | any): Promise<STX> {
  const {sk, AlgoSigner} = networkAccount;
  if (sk) {
    const tx = txnOrig.signTxn(sk);
    const ret = {
      tx,
      txID: txnOrig.txID().toString(),
      lastRound: txnOrig.lastRound,
    }
    return ret;
  } else if (AlgoSigner) {
    // TODO: clean up txn before signing
    const txn = clean_for_AlgoSigner(txnOrig);

    // XXX the following comment is dead code that was previously used for debugging purposes.
    // It is left here for now because it might be useful later.
    // But this should eventually be deleted

    // if (sk) {
    //   const re_tx = txnOrig.signTxn ? txnOrig : new algosdk__src__transaction.Transaction(txnOrig);
    //   re_tx.group = txnOrig.group;

    //   const sk_tx = re_tx.signTxn(sk);
    //   const sk_ret = {
    //     tx: sk_tx,
    //     txID: re_tx.txID().toString(),
    //     lastRound: txnOrig.lastRound,
    //   }
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

    // debug('signed AlgoSigner')
    // debug({txID: ret.txID});
    // debug(msgpack.decode(ret.tx));
    return ret;
  } else {
    throw Error(`networkAccount has neither sk nor AlgoSigner: ${JSON.stringify(networkAccount)}`);
  }
}

export const connectAccount = async (networkAccount: NetworkAccount) => {
  const indexer = await getIndexer();
  const thisAcc = networkAccount;
  const shad = thisAcc.addr.substring(2, 6);
  const pks = T_Address.canonicalize(thisAcc);
  debug(`${shad}: connectAccount`);

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

  const attachP = async (bin: Backend, ctcInfoP: Promise<ContractInfo>): Promise<ContractAttached> => {
    const ctcInfo = await ctcInfoP;
    const getInfo = async () => ctcInfo;
    const ApplicationID = ctcInfo.ApplicationID;
    let lastRound = ctcInfo.creationRound;
    debug(`${shad}: attach ${ApplicationID} created at ${lastRound}`);

    const bin_comp = await compileFor(bin, ApplicationID);
    await verifyContract(ctcInfo, bin);
    const ctc_prog = algosdk.makeLogicSig(bin_comp.ctc.result, []);

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      return await waitUntilTime(bigNumberify(lastRound).add(delta));
    };

    const sendrecv = async (
      label: string,
      funcNum: number,
      evt_cnt: number,
      hasLastTime: (BigNumber | false),
      tys: Array<AnyALGO_Ty>,
      args: Array<any>,
      value: BigNumber,
      out_tys: Array<AnyALGO_Ty>,
      onlyIf: boolean,
      soloSend: boolean,
      timeout_delay: undefined | BigNumber,
      sim_p: (fake: Recv) => SimRes,
    ): Promise<Recv> => {
      if ( hasLastTime !== false ) {
        const ltidx = hasLastTime.toNumber();
        tys.splice(ltidx, 1);
        args.splice(ltidx, 1);
      }
      const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
        await recv(label, funcNum, evt_cnt, out_tys, waitIfNotPresent, timeout_delay);
      if ( ! onlyIf ) {
        return await doRecv(true);
      }

      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} sendrecv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const fake_res = {
        didTimeout: false,
        data: argsSlice(args, evt_cnt),
        time: bigNumberify(0), // This should not be read.
        value: value,
        from: pks,
      };
      const sim_r = sim_p( fake_res );
      debug(`${dhead} --- SIMULATE ${JSON.stringify(sim_r)}`);
      const isHalt = sim_r.isHalt;
      const sim_txns = sim_r.txns;

      while ( true ) {
        const params = await getTxnParams();
        if ( timeout_delay ) {
          const tdn = timeout_delay.toNumber();
          params.lastRound = lastRound + tdn;
          if ( params.firstRound > params.lastRound ) {
            debug(`${dhead} --- FAIL/TIMEOUT`);
            return {didTimeout: true};
          }
        }

        debug(`${dhead} --- ASSEMBLE w/ ${JSON.stringify(params)}`);

        const txnFromContracts =
          sim_txns.map(
            (txn_nfo: SimTxn) =>
            algosdk.makePaymentTxnWithSuggestedParams(
              bin_comp.ctc.hash,
              // XXX use some other function
              algosdk.encodeAddress(Buffer.from(txn_nfo.to.slice(2), 'hex')),
              txn_nfo.amt.toNumber(),
              undefined, ui8z,
              params));
        const totalFromFee =
          txnFromContracts.reduce(((sum: number, txn: Txn): number => sum + txn.fee), 0);
        debug(`${dhead} --- totalFromFee = ${JSON.stringify(totalFromFee)}`);

        debug(`${dhead} --- isHalt = ${JSON.stringify(isHalt)}`);

        const actual_args =
        [ sim_r.prevSt_noPrevTime, sim_r.nextSt_noTime, isHalt, bigNumberify(totalFromFee), lastRound, ...args ];
      const actual_tys =
        [ T_Digest, T_Digest, T_Bool, T_UInt, T_UInt, ...tys ];
      debug(`${dhead} --- ARGS = ${JSON.stringify(actual_args)}`);

      const safe_args: Array<NV> = actual_args.map((m, i) => actual_tys[i].toNet(m));
      safe_args.forEach((x) => {
        if (! ( x instanceof Uint8Array ) ) {
          // The types say this is impossible now,
          // but we'll leave it in for a while just in case...
          throw Error(`expect safe program argument, got ${JSON.stringify(x)}`);
        }
      });
      const ui8h = (x:Uint8Array): string => Buffer.from(x).toString('hex');
      debug(`${dhead} --- PREPARE: ${JSON.stringify(safe_args.map(ui8h))}`);

      const handler_with_args =
        algosdk.makeLogicSig(handler.result, safe_args);
      debug(`${dhead} --- PREPARED`); // XXX display handler_with_args usefully, like with base64ify toBytes

        const whichAppl =
          isHalt ?
          // We are treating it like any party can delete the application, but the docs say it may only be possible for the creator. The code appears to not care: https://github.com/algorand/go-algorand/blob/0e9cc6b0c2ddc43c3cfa751d61c1321d8707c0da/ledger/apply/application.go#L589
          algosdk.makeApplicationDeleteTxn :
          algosdk.makeApplicationNoOpTxn;
        // XXX if it is a halt, generate closeremaindertos for all the handlers and the contract account
        const txnAppl =
          whichAppl(
            thisAcc.addr, params, ApplicationID);
        const txnFromHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            handler.hash,
            thisAcc.addr,
            0, undefined, ui8z,
            params);
        debug(`${dhead} --- txnFromHandler = ${JSON.stringify(txnFromHandler)}`);
        const txnToHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            handler.hash,
            txnFromHandler.fee,
            undefined, ui8z,
            params);
        debug(`${dhead} --- txnToHandler = ${JSON.stringify(txnToHandler)}`);
        const txnToContract =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            bin_comp.ctc.hash,
            value.toNumber() + totalFromFee,
            undefined, ui8z,
            params);
        const txns = [
          txnAppl,
          txnToHandler,
          txnFromHandler,
          txnToContract,
          ...txnFromContracts ];
        algosdk.assignGroupID(txns);
        regroup(thisAcc, txns);

        const signLSTO = (txn: Txn, ls: any): STX => {
          const tx_obj = algosdk.signLogicSigTransactionObject(txn, ls);
          return {
            tx: tx_obj.blob,
            txID: tx_obj.txID,
            lastRound: txn.lastRound,
          }
        }
        const sign_me = async (x: Txn): Promise<STX> => await signTxn(thisAcc, x);

        const txnAppl_s = await sign_me(txnAppl);
        const txnFromHandler_s = signLSTO(txnFromHandler, handler_with_args);
        // debug(`txnFromHandler_s: ${base64ify(txnFromHandler_s)}`);
        const txnToHandler_s = await sign_me(txnToHandler);
        const txnToContract_s = await sign_me(txnToContract);
        const txnFromContracts_s =
          txnFromContracts.map((txn: Txn) => signLSTO(txn, ctc_prog));

        const txns_s = [
          txnAppl_s,
          txnToHandler_s,
          txnFromHandler_s,
          txnToContract_s,
          ...txnFromContracts_s
        ];

        debug(`${dhead} --- SEND: ${txns_s.length}`);
        let res;
        try {
          res = await sendAndConfirm( txns_s );

          // XXX we should inspect res and if we failed because we didn't get picked out of the queue, then we shouldn't error, but should retry and let the timeout logic happen.
          debug(`${dhead} --- SUCCESS: ${JSON.stringify(res)}`);
        } catch (e) {
          const handle_error =
            (!soloSend) ? debug : ((x:string) => { throw Error(x); });

          if ( e.type == "sendRawTransaction" ) {
            handle_error(`${dhead} --- FAIL:\n${format_failed_request(e.e)}`);
          } else {
            handle_error(`${dhead} --- FAIL:\n${JSON.stringify(e)}`);
          }
        }

        return await doRecv(false);
      }
    };

    const recv = async (
      label: string,
      funcNum: number,
      evt_cnt: number,
      tys: Array<AnyALGO_Ty>,
      waitIfNotPresent: boolean,
      timeout_delay: undefined | BigNumber
    ): Promise<Recv> => {
      // Ignoring this, because no ALGO dev node
      void(waitIfNotPresent);

      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} recv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);

      const handler = bin_comp.steps[funcNum];
      if ( ! handler ) {
        throw Error(`${dhead} Internal error: reference to undefined handler: ${funcName}`); }

      const timeoutRound =
        timeout_delay ?
        lastRound + timeout_delay.toNumber() :
        undefined;

      while ( true ) {
        const currentRound = await getLastRound();
        if ( timeoutRound && timeoutRound < currentRound ) {
          return { didTimeout: true };
        }

        let query = indexer.searchForTransactions()
          .address(handler.hash)
          .addressRole("sender")
          // Look at the next one after the last message
          // XXX when we implement firstMsg, this won't work on the first
          // message
          .minRound(lastRound + 1);
        if ( timeoutRound ) {
          query = query.maxRound(timeoutRound);
        }

        const txn = await doQuery(dhead, query);
        if ( ! txn ) {
          // XXX perhaps wait until a new round has happened using wait
          await Timeout.set(2000);
          continue;
        }

        const ctc_args: Array<string> =
          txn.signature.logicsig.args;
        debug(`${dhead} --- ctc_args = ${JSON.stringify(ctc_args)}`);

        const args = argsSlice(ctc_args, evt_cnt);
        debug(`${dhead} --- args = ${JSON.stringify(args)}`);

        /** @description base64->hex->arrayify */
        const reNetify = (x: string): NV => {
          const s: string = Buffer.from(x, 'base64').toString('hex');
          // debug(`${dhead} --- deNetify ${s}`);
          return ethers.utils.arrayify('0x' + s);
        }

        const args_un =
            args.map((x, i) => tys[i].fromNet(reNetify(x)));
        debug(`${dhead} --- args_un = ${JSON.stringify(args_un)}`);

        const totalFromFee =
          T_UInt.fromNet(reNetify(ctc_args[3]));
        debug(`${dhead} --- totalFromFee = ${JSON.stringify(totalFromFee)}`);

        const fromAddr =
          txn['payment-transaction'].receiver;
        const from =
          T_Address.canonicalize({addr: fromAddr});
        debug(`${dhead} --- from = ${JSON.stringify(from)} = ${fromAddr}`);

        const oldLastRound = lastRound;
        lastRound = txn['confirmed-round'];
        debug(`${dhead} --- updating round from ${oldLastRound} to ${lastRound}`);

        // XXX ideally we'd get the whole transaction group before and not need to do this.
        const ptxn =
          await doQuery(
            dhead,
            indexer.searchForTransactions()
              .address(bin_comp.ctc.hash)
              .addressRole("receiver")
              .round(lastRound));

        const value =
          bigNumberify(ptxn['payment-transaction'].amount)
            .sub(totalFromFee);
        debug(`${dhead} --- value = ${JSON.stringify(value)}`);

        return {
          didTimeout: false,
          data: args_un,
          time: bigNumberify(lastRound),
          value, from,
        };
      }
    };

    const creationTime = async () => bigNumberify((await getInfo()).creationRound);

    return { getInfo, creationTime, sendrecv, recv, iam, selfAddress, wait, stdlib: compiledStdlib };
  };

  const deployP = async (bin: Backend): Promise<ContractAttached> => {
    must_be_supported(bin);
    debug(`${shad} deploy`);
    const algob = bin._Connectors.ALGO;

    const { appApproval0, appClear } = algob;

    const appApproval0_subst =
      replaceAddr('Deployer', thisAcc.addr, appApproval0);
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
          0, 0, 2, 1));

    const ApplicationID = createRes["application-index"];
    if ( ! ApplicationID ) {
      throw Error(`No application-index in ${JSON.stringify(createRes)}`);
    }
    const bin_comp = await compileFor(bin, ApplicationID);

    const params = await getTxnParams();
    const txnUpdate =
      algosdk.makeApplicationUpdateTxn(
        thisAcc.addr, params,
        ApplicationID, bin_comp.appApproval.result,
        appClear_bin.result);
    const txnToContract =
      algosdk.makePaymentTxnWithSuggestedParams(
        thisAcc.addr,
        bin_comp.ctc.hash,
        raw_minimumBalance,
        undefined, ui8z,
        params);
    const txnToHandlers: Array<Txn> =
      bin_comp.steps.flatMap((sc: CompileResultBytes|null): Array<Txn> => {
        if ( ! sc) { return []; }
        return [algosdk.makePaymentTxnWithSuggestedParams(
          thisAcc.addr,
          sc.hash,
          raw_minimumBalance,
          undefined, ui8z,
          params)];
    });

    const txns = [
      txnUpdate,
      txnToContract,
      ...txnToHandlers
    ];
    algosdk.assignGroupID(txns);
    regroup(thisAcc, txns);

    const txnUpdate_s =
      await signTxn(thisAcc, txnUpdate);
    const txnToContract_s =
      await signTxn(thisAcc, txnToContract);

    // This is written a dumb way to make sure signatures happen one at a time.
    // AlgoSigner errors if multiple sigs are trying to happen at the same time.
    const txnToHandlers_s: Array<STX> = [];
    for (const tx of txnToHandlers) {
      txnToHandlers_s.push(await signTxn(thisAcc, tx));
    }

    const txns_s = [
      txnUpdate_s,
      txnToContract_s,
      ...txnToHandlers_s,
    ];

    let updateRes;
    try {
        updateRes = await sendAndConfirm( txns_s );
    } catch (e) {
      throw Error(`deploy: ${JSON.stringify(e)}`);
    }

    const creationRound = updateRes['confirmed-round'];
    const getInfo = async (): Promise<ContractInfo> =>
      ({ ApplicationID, creationRound });

    debug(`${shad} application created`);
    return await attachP(bin, getInfo());
  };

  /**
   * @description Push await down into the functions of a ContractAttached
   * @param implP A promise of an implementation of ContractAttached
   */
  const deferP = (implP: Promise<ContractAttached>): ContractAttached => {
    return {
      getInfo: async () => (await implP).getInfo(),
      creationTime: async () => (await implP).creationTime(),
      sendrecv: async (...args: any) => (await implP).sendrecv(...args),
      recv: async (...args: any) => (await implP).recv(...args),
      wait: async(...args: any) => (await implP).wait(...args),
      iam, // doesn't need to await the implP
      selfAddress, // doesn't need to await the implP
      stdlib: compiledStdlib,
    }
  };

  const attach = (bin: Backend, ctcInfoP: Promise<ContractInfo>): ContractAttached => {
    return deferP(attachP(bin, ctcInfoP));
  };

  const deploy = (bin: Backend): ContractAttached => {
    return deferP(deployP(bin));
  };
  return { deploy, attach, networkAccount, stdlib: compiledStdlib };
};

export const balanceOf = async (acc: Account): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  const client = await getAlgodClient();
  const {amount} = await client.accountInformation(networkAccount.addr).do();
  return bigNumberify(amount);
};


export const createAccount = async () => {
  const networkAccount = algosdk.generateAccount();
  return await connectAccount(networkAccount);
}

export const fundFromFaucet = async (account: Account, value: any) => {
  const faucet = await getFaucet();
  await transfer(faucet, account, value);
}

export const newTestAccount = async (startingBalance: any) => {
  const account = await createAccount();
  if (getDEBUG()) { await showBalance('before', account.networkAccount); }
  await fundFromFaucet(account, startingBalance);
  if (getDEBUG()) { await showBalance('after', account.networkAccount); }
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
  // @ts-ignore
  const mnemonic = window.prompt('Please paste the mnemonic for your account');
  if (mnemonic) {
    return await newAccountFromMnemonic(mnemonic);
  } else {
    throw Error(`User declined to provide a mnemonic`);
    // XXX: figure out how to let the user pick which wallet they want to use.
    // AlgoSigner, My Algo Wallet, etc.
    // throw Error(`Please use newAccountFromAlgoSigner instead`);
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
}

export const getNetworkTime = async () => bigNumberify(await getLastRound());

export const waitUntilTime = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg = onProgress || (() => {});
  let currentTime = await getNetworkTime();
  while (currentTime.lt(targetTime)) {
    debug(`waitUntilTime: iteration: ${currentTime} -> ${targetTime}`);
    const status = await (await getAlgodClient()).statusAfterBlock(currentTime.toNumber()).do();
    currentTime = bigNumberify(status['last-round']);
    onProg({currentTime, targetTime});
  }
  debug(`waitUntilTime: ended: ${currentTime} -> ${targetTime}`);
  return currentTime;
};

export const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  debug(`wait: delta=${delta} now=${now}, until=${now.add(delta)}`);
  return await waitUntilTime(now.add(delta), onProgress);
};

// XXX: implement this
export const verifyContract = async (ctcInfo: ContractInfo, backend: Backend): Promise<true> => {
  void(ctcInfo);
  void(backend);

  // XXX verify contract was deployed at creationRound
  // XXX verify something about ApplicationId

  // XXX (above) attach creator info to ContractInfo
  // XXX verify creator was the one that deployed the contract

  // XXX verify deployed contract code matches backend

  // (after deployMode:firstMsg is implemented)
  // XXX (above) attach initial args to ContractInfo
  // XXX verify contract storage matches expectations based on initial args
  // (don't bother checking ctc balance at creationRound, the ctc enforces this)

  return true;
}
