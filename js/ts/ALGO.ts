// XXX: do not import any types from algosdk; instead copy/paste them below
// XXX: can stop doing this workaround once @types/algosdk is shippable
import algosdk from 'algosdk';
import base32 from 'hi-base32';
import { BigNumber } from 'ethers';

import {
  CurrencyAmount, debug,
  isBigNumber, bigNumberify,
  getDEBUG, setDEBUG } from './shared';
export * from './shared';

setDEBUG(true);

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX Copy/pasted type defs from types/algosdk
// This is so that this module can be exported without our custom types/algosdk
// The unused ones are commented out
type Round = number
type Address = string
type SecretKey = Uint8Array // length 64
  // TODO: find the proper algo terminology for Wallet
type Wallet = {
    addr: Address,
    sk: SecretKey, // TODO: describe length? (64)
  }
type SignedTxn = {
    opaque: undefined // TODO
  }
type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
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
  result: Uint8Array,
  hash: Address
};

type NetworkAccount = Wallet;
type Account = {
  networkAccount: NetworkAccount,
};

type BoolString = 'True' | 'False';
type Backend = {_Connectors: {ALGO: {
  appApproval0: string,
  appApproval: string,
  appClear: string,
  ctc: string,
  steps: string,
  // m1 -- mN
  [key: string]: string,
  unsupported: BoolString,
}}};

type Digest = BigNumber;
type SimRes = {
  prevSt: Digest,
  txns: Array<SimTxn>,
  nextSt: Digest,
  isHalt : boolean,
};
type SimTxn = {
  to: Address,
  amt: BigNumber,
};

type CompiledBackend = {
  appApproval: CompileResultBytes,
  appClear: CompileResultBytes,
  ctc: CompileResultBytes,
  steps: { [key: string]: CompileResultBytes },
};

type ContractAttached = {
  getInfo: () => Promise<ContractInfo>,
  sendrecv: (...argz: any) => any,
  recv: (...argz: any) => any,
  wait: (...argz: any) => any,
  iam: (some_addr: Address) => Address,
};

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

// Common interface exports

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
const token = process.env.ALGO_TOKEN || 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const server = process.env.ALGO_SERVER || 'http://localhost';
const port = process.env.ALGO_PORT || 4180;
const algodClient = new algosdk.Algodv2(token, server, port);

// eslint-disable-next-line max-len
const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || 'pulp abstract olive name enjoy trick float comfort verb danger eternal laptop acquire fetch message marble jump level spirit during benefit sure dry absent history'));
// if using the default:
// assert(FAUCET.addr === 'EYTSJVJIMJDUSRRNTMVLORTLTOVDWZ6SWOSY77JHPDWSD7K3P53IB3GUPQ');

// Helpers

const getLastRound = async (): Promise<Round> =>
  (await algodClient.status().do())['last-round'];

const waitForConfirmation = async (txId: TxId, untilRound: number): Promise<null | TxnInfo> => {
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
  return null;
};

const sendAndConfirm = async (
  label:string, stx_or_stxs: SignedTxn | Array<SignedTxn>, txn: Txn
): Promise<null | TxnInfo> => {
  const txID = txn.txID().toString();
  const untilRound = txn.lastRound;
  try {
    await algodClient.sendRawTransaction(stx_or_stxs).do();
  } catch (e) {
    throw Error(`sendAndConfirm ${label} failed:\n${JSON.stringify(e)}\n\ton\n${JSON.stringify(stx_or_stxs)}`);
  }
  return await waitForConfirmation(txID, untilRound);
};

// // Backend
const compileTEAL = async (label: string, code: string): Promise<CompileResultBytes> => {
  debug(`compile ${label}`)
  let s, r;
  try {
    r = await algodClient.compile(code).do();
    s = 200;
  } catch (e) {
    s = e.statusCode;
    r = e;
  }

  if ( s == 200 ) {
    debug(`compile ${label} succeeded: ${JSON.stringify(r)}`);
    r.result = new Uint8Array(Buffer.from(r.result, "base64"));
    // debug(`compile transformed: ${JSON.stringify(r)}`);
    return r;
  } else {
    throw Error(`compile ${label} failed: ${s}: ${JSON.stringify(r)}`);
  }
};

const getTxnParams = async (): Promise<TxnParams> => {
  debug(`fillTxn: getting params`);
  const params = await algodClient.getTransactionParams().do();
  debug(`fillTxn: got params: ${JSON.stringify(params)}`);
  return params;
};

const sign_and_send_sync = async (
  label: string,
  sk: SecretKey,
  txn: Txn,
): Promise<TxnInfo> => {
  const txn_s = txn.signTxn(sk);
  const res = await sendAndConfirm(label, txn_s, txn);
  if ( ! res ) {
    throw Error(`${label} txn failed: ${JSON.stringify(txn)}`);
  }
  return res;
};

// const fillTxn = async (round_width, txn) => {
//   return fillTxnWithParams(false, round_width, await getTxnParams(), txn);
// };

export const transfer = async (from: Account, to: Account, value: BigNumber): Promise<TxnInfo> => {
  const valuen = value.toNumber();
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;

  const note = algosdk.encodeObj('@reach-sh/ALGO.mjs transfer');
  return await sign_and_send_sync(
    `transfer ${from} ${to} ${valuen}`,
    sender.sk,
    algosdk.makePaymentTxnWithSuggestedParams(
      sender.addr, receiver, valuen, undefined, note, await getTxnParams(),
    ));
};

// XXX I'd use replaceAll if I could, but I'm pretty sure there's just one occurrence. It would be better to extend ConnectorInfo so these are functions
const replaceAddr = (label: string, addr: Address, x:string): string =>
x.replace(`"{{${label}}}"`,
          `base32(${base32.encode(algosdk.addressPublicKey(addr)).toString()})`);

function must_be_supported(bin: Backend) {
  const algob = bin._Connectors.ALGO;
  const { unsupported } = algob;
  if ( unsupported == `True` ) {
    throw Error(`This Reach application is not supported on Algorand.`);
  }
}

async function compileFor(bin: Backend, ApplicationID: number): Promise<CompiledBackend> {
  must_be_supported(bin);
  const algob = bin._Connectors.ALGO;

  const { appApproval, appClear, ctc, steps } = algob;
  const stepsN = parseInt(steps);
  const stepCode: { [key: string]: string } = {};
  for ( let i = 1; i <= stepsN; i++ ) {
    const key = `m${i}`
    stepCode[key] = algob[key];
    if ( !stepCode[key] ) {
      throw Error(`Expected ${key} in ${JSON.stringify(algob)}`);
    }
  }

  const subst_appid = (x: string) => x.replace('{{ApplicationID}}', ApplicationID.toString());

  const ctc_bin = await compileTEAL('ctc_subst', subst_appid(ctc));
  const subst_ctc = (x: string) => replaceAddr('ContractAddr', ctc_bin.hash, x);

  const stepCode_bin: { [key: string]: CompileResultBytes } = {};
  let appApproval_subst = appApproval;
  for ( const mN in stepCode  ) {
    const mc = stepCode[mN];
    const mc_subst = subst_ctc(subst_appid(mc));
    const cr = await compileTEAL(mN, mc_subst);
    stepCode_bin[mN] = cr;
    appApproval_subst =
      replaceAddr(mN, cr.hash, appApproval_subst);
  }

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

export const connectAccount = async (networkAccount: NetworkAccount) => {
  const thisAcc = networkAccount;
  const shad = thisAcc.addr.substring(2, 6);
  debug(`${shad}: connectAccount`);

  const attach = async (bin: Backend, ctcInfoP: Promise<ContractInfo>): Promise<ContractAttached> => {
    const ctcInfo = await ctcInfoP;
    const getInfo = async () => ctcInfo;
    const ApplicationID = ctcInfo.ApplicationID;
    let lastRound = ctcInfo.creationRound;
    debug(`${shad}: attach ${ApplicationID} created at ${lastRound}`);

    const bin_comp = await compileFor(bin, ApplicationID);
    const ctc_prog = algosdk.makeLogicSig(bin_comp.ctc.result, []);

    const iam = (some_addr: Address): Address => {
      if (some_addr == thisAcc.addr) {
        return thisAcc.addr;
      } else {
        throw Error(`I should be ${some_addr}, but am ${thisAcc.addr}`);
      }
    };

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      void(delta);
      throw Error(`XXX Not implemented: wait`);
    };

    const sendrecv = async (
      label: string,
      funcNum: number,
      evt_cnt: number, 
      tys: Array<any>,
      args: Array<any>,
      value: BigNumber,
      out_tys: Array<any>,
      timeout_delay: undefined | BigNumber,
      sim_p: () => SimRes,
    ) => {
      const funcName = `m${funcNum}`;
      debug(`${shad}: ${label} sendrecv ${funcName} ${timeout_delay} --- START`);
      const handler = bin_comp.steps[funcName];

      const sim_r = sim_p();
      const isHalt = sim_r.isHalt;
      const sim_txns = sim_r.txns;

      const munged_args =
        // XXX this needs to be customized for Algorand
        args.map((m, i) => tys[i].munge(tys[i].canonicalize(m)));
      const actual_args =
        [ sim_r.prevSt, sim_r.nextSt, isHalt, lastRound, ...munged_args ];

      debug(`${shad}: ${label} sendrecv ${funcName} ${timeout_delay} --- PREPARE w/ ${JSON.stringify(actual_args)}`);
      const handler_with_args =
        algosdk.makeLogicSig(handler.result, actual_args);

      while ( true ){
        const params = await getTxnParams();
        if ( timeout_delay ) {
          const tdn = timeout_delay.toNumber();
          params.lastRound = lastRound + tdn;
          if ( params.firstRound > params.lastRound ) {
            debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- FAIL/TIMEOUT`);
            return {didTimeout: true};
          }
        }

        debug(`${shad}: ${label} sendrecv ${funcName} ${timeout_delay} --- ASSEMBLE w/ ${JSON.stringify(params)}`);
        const whichAppl =
          isHalt ?
          // We are treating it like any party can delete the application, but the docs say it may only be possible for the creator. The code appears to not care: https://github.com/algorand/go-algorand/blob/0e9cc6b0c2ddc43c3cfa751d61c1321d8707c0da/ledger/apply/application.go#L589
          algosdk.makeApplicationDeleteTxn :
          algosdk.makeApplicationNoOpTxn;
        const txnAppl =
          whichAppl(
            thisAcc.addr, params, ApplicationID);
        const txnFromHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            handler.hash, 
            thisAcc.addr,
            0, undefined, ui8z,
            params);
        const txnToHandler =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            handler.hash,
            params.fee,
            undefined, ui8z,
            params);
        const txnToContract =
          algosdk.makePaymentTxnWithSuggestedParams(
            thisAcc.addr,
            bin_comp.ctc.hash,
            value.toNumber(),
            undefined, ui8z,
            params);
        const txnFromContracts =
          sim_txns.map(
            (txn_nfo: SimTxn) =>
            algosdk.makePaymentTxnWithSuggestedParams(
              bin_comp.ctc.hash,
              txn_nfo.to,
              txn_nfo.amt.toNumber(),
              undefined, ui8z,
              params));
        const txns = [
          txnAppl, 
          txnFromHandler,
          txnToHandler,
          txnToContract,
          ...txnFromContracts ];
        algosdk.assignGroupID(txns);

        const sign_me = (x: Txn) => x.signTxn(thisAcc.sk);

        const txnAppl_s = sign_me(txnAppl);
        const txnFromHandler_s =
          algosdk.signLogicSigTransactionObject(
            txnFromHandler, handler_with_args);
        const txnToHandler_s = sign_me(txnToHandler);
        const txnToContract_s = sign_me(txnToContract);
        const txnFromContracts_s =
          txnFromContracts.map(
            (txn: Txn) => 
            algosdk.signLogicSigTransactionObject(txn, ctc_prog));

        const txns_s = [
          txnAppl_s,
          txnFromHandler_s,
          txnToHandler_s,
          txnToContract_s,
          ...txnFromContracts_s ];

        debug(`${shad}: ${label} sendrecv ${funcName} ${timeout_delay} --- SEND`);
        const res = await sendAndConfirm(
          `${shad}: ${label} sendrecv ${funcName}`,
          txns_s,
          txnAppl
        );
        if ( ! res ) {
          // XXX we should inspect res and if we failed because we didn't get picked out of the queue, then we shouldn't error, but should retry and let the timeout logic happen.
          throw Error(`${shad}: ${label} sendrecv ${funcName} ${timeout_delay} --- FAIL: ${JSON.stringify(res)}`);
        }

        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      }

//       do {
//         const confirmedTxn = await sendsAndConfirm(signedTxns, appTxn.lastRound);
//         if (confirmedTxn) {
//           debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- OKAY`);
//           // FIXME no documentation on whether confirmedTxn has something for each txn in a group or only one thing. We made need to call returnFromTxn with appTxn for the data and confirmedTxn for the round/etc
//           return (await returnFromTxn(confirmedTxn, evt_cnt));
//         }

//         if (!this_is_a_timeout) {
//           debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- FAIL/TIMEOUT`);
//           const rec_res = await recv(label, timeNum, 0, false, false, false, false, false);
//           rec_res.didTimeout = true;
//           return rec_res;
//         }
//       }
//       while (this_is_a_timeout);
    };

    const recv = async (
      label: string,
      funcNum: number, 
      ok_cnt: number,
      out_tys: Array<any>,
      timeout_delay: undefined | BigNumber
    ) => {
      void(label);
      void(funcNum);
      void(ok_cnt);
      void(out_tys);
      void(timeout_delay);

//       debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- START`);

//       const this_is_a_timeout = timeout_delay ? false : true;
//       if (this_is_a_timeout) {
//         timeout_delay = default_range_width;
//       }

//       while (1) {
//         const startRound = this_is_a_timeout ? prevRound : await getLastRound();
//         const untilRound = prevRound + timeout_delay;
//         while ((await getLastRound()) < untilRound) {
//           // FIXME maxj says there will be a better query api in the future, when that is in place, push this forEach to the indexer
//           const resp = await algodClient.transactionByAddress(ctc.address, startRound, untilRound).do();
//           resp.transactions.forEach(async txn => {
//             if (txn.type == 'appl' &&
//               txn.ApplicationId == ctc.appId &&
//               txn.ApplicationArgs[0] == okNum &&
//               txn.ApplicationArgs[1] == prevRound) {
//               debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- OKAY`);
//               return (await returnFromTxn(txn, ok_cnt));
//             }
//           });
//         }

//         if (!this_is_a_timeout) {
//           debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- TIMEOUT`);
//           const rec_res = timeout_me ?
//             await sendrecv(label, timeNum, 0, timeout_args, 0, false, false, try_p) :
//             await recv(label, timeNum, 0, false, false, false, false, false);
//           rec_res.didTimeout = true;
//           return rec_res;
//         }
//       }
      throw Error(`XXX recv`);
    };

    return { getInfo, sendrecv, recv, iam, wait };
  };

  const deploy = async (bin: Backend): Promise<ContractAttached> => {
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
        thisAcc.sk,
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

    const updateRes =
      await sign_and_send_sync(
        'ApplicationUpdate',
        thisAcc.sk,
        algosdk.makeApplicationUpdateTxn(
          thisAcc.addr, await getTxnParams(),
          ApplicationID, bin_comp.appApproval.result,
          appClear_bin.result));

    const creationRound = updateRes['confirmed-round'];
    const getInfo = async (): Promise<ContractInfo> =>
      ({ ApplicationID, creationRound });

    debug(`${shad} application created`);
    return await attach(bin, getInfo());
  };
//   const deploy = async (bin) => {

//     debug(`${shad}: deploy: making account`);
//     const ctc_acc = algosdk.generateAccount();

//     debug(`${shad}: deploy: filling transaction`);
//     const appTxn = await fillTxn(default_range_width, {
//       // FIXME JS SDK doesn't handle this kind of txn
//       'from': thisAcc.addr,
//       'type': 'appl',
//       'ApplicationId': 0,
//       'OnCompletion': 'noOp',
//       'ApplicationArgs': [],
//       'ApprovalProgram': ApprovalProgram,
//       'ClearStateProgram': ClearStateProgram,
//       'GlobalStateSchema': { 'NumByteSlice': 2 },
//       'Accounts': [ctc_acc.addr],
//       // FIXME: Use note field for link to Reach code
//     });
//     debug(`${shad}: deploy: signing transction`);
//     const signedTxn = algosdk.signTransaction(appTxn, thisAcc.sk);
//     debug(`${shad}: deploy: txn signed, sending`);
//     const confirmedTxn = await sendAndConfirm(signedTxn);
//     debug(`${shad}: deploy: application confirmed`);
//     const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
//     const creationRound = confirmedTxn.round;

//     // FIXME relies on https://github.com/algorand/go-algorand/issues/1051 fix
//     const logic_sig = algosdk.makeLogicSig(Buffer.from(LogicSigProgram, 'base64'), [appId]);
//     logic_sig.sign(ctc_acc.sk);

//     const ctc = { address: ctc_acc.addr, appId, creationRound, logic_sig };

//     return attach(bin, ctc);
//   };

  return { deploy, attach, networkAccount };
};

const getBalanceAt = async (addr: Address, round: Round): Promise<number> => {
  void(round);
  // FIXME: Don't ignore round, but this requires 'the next indexer version' (Max on 2020/05/05)
  return (await algodClient.accountInformation(addr).do()).amount;
};

export const balanceOf = async (acc: Account) => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  return (await getBalanceAt(networkAccount.addr, await getLastRound()));
};

const showBalance = async (note: string, networkAccount: NetworkAccount) => {
  const bal = await balanceOf({ networkAccount });
  const showBal = algosdk.microalgosToAlgos(bal).toFixed(2);
  console.log('%s: balance: %s algos', note, showBal);
};

export const newTestAccount = async (startingBalance: BigNumber) => {
  const networkAccount = algosdk.generateAccount();
  if (getDEBUG()) { await showBalance('before', networkAccount); }
  await transfer({networkAccount: FAUCET}, {networkAccount}, startingBalance);
  if (getDEBUG()) { await showBalance('after', networkAccount); }
  return await connectAccount(networkAccount);
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

/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100000000')); // => '100'
 */
export function formatCurrency(amt: BigNumber, decimals: number = 6): string {
  // Recall that 1 algo = 10^6 microalgos
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Use decimals+1 and then slice it off to truncate instead of round
  const algosStr = algosdk.microalgosToAlgos(amt.toNumber()).toFixed(decimals+1);
  // Have to roundtrip thru Number to drop trailing zeroes
  return Number(algosStr.slice(0, algosStr.length - 1)).toString();
}

export const newAccountFromMnemonic = false; // XXX
export const getNetworkTime = getLastRound;
export const waitUntilTime = false; // XXX
export const wait = false; // XXX
export const verifyContract = false; // XXX
