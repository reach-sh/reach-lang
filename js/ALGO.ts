import algosdk, { Address, ApiCall, Round, SignedTxn, StatusInfo, TxId, Txn, TxnInfo, TxnParams, Wallet } from 'algosdk';

import { CurrencyMap, debug, isBigNumber } from './shared';
export * from './shared';

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict


type NetworkAccount = Wallet;
type Account = {
  networkAccount: NetworkAccount,
}

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

const log_pretty = (obj: any): void =>
  console.log(JSON.parse(JSON.stringify(obj)));

// Common interface exports

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
const token = process.env.ALGO_TOKEN || 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const server = process.env.ALGO_SERVER || 'http://localhost';
const port = process.env.ALGO_PORT || 4180;
const algodClient = new algosdk.Algodv2(token, server, port);

const default_range_width = 1000;
void(default_range_width); // XXX

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

// XXX Untested but something like this should work
// [{signedTxn, txn}] -> ()
const sendsAndConfirm = async (
  txns: Array<{signedTxn: SignedTxn, txn: Txn}>
): Promise<Array<null | TxnInfo>> => {
  const signedTxns = txns.map(pair => pair.signedTxn);
  await algodClient.sendRawTransactions(signedTxns).do();
  return Promise.all([
    ...txns.map(pair => waitForConfirmation(pair.txn.txID().toString(), pair.txn.lastRound)),
  ]);
  // // FIXME https://developer.algorand.org/docs/features/atomic_transfers/ ... Send transactions ... claims that tx has a txId field, but it doesn't as far as I can tell, because this crashes.
  // const untilRound = Math.max(...txns.map(pair => pair.txn.lastRound));
  // const tx = await algodClient.sendRawTransactions(signedTxns).do();
  // return (await waitForConfirmation(tx.txID, untilRound));
};
void(sendsAndConfirm); // XXX

const sendAndConfirm = async (
  signedTxn: SignedTxn, txn: Txn
): Promise<null | TxnInfo> => {
  const txID = txn.txID().toString();
  const untilRound = txn.lastRound;
  await algodClient.sendRawTransaction(signedTxn).do();
  return await waitForConfirmation(txID, untilRound);
};

// // Backend
// const fillTxnWithParams = (firstRound, round_width, params, txn0) => {
//   // Don't need these anymore?
//   void(round_width);
//   void(firstRound);
//   // const theFirstRound = firstRound ? firstRound : params.lastRound;
//   // FIXME these fields don't match the documentation https://developer.algorand.org/docs/reference/transactions/ so update once they fix this issue https://github.com/algorand/js-algorand-sdk/issues/144
//   // Fixed?
//   // txn.flatFee = params.flatFee;
//   // txn.fee = params.fee;
//   // txn.firstRound = theFirstRound;
//   // txn.lastRound = theFirstRound + round_width;
//   // txn.genesisID = params.genesisID;
//   // txn.genesisHash = params.genesisHash;
//   return {
//     ...params,
//     ...txn0,
//   };
// };

const getTxnParams = async (): Promise<TxnParams> => {
  debug(`fillTxn: getting params`);
  const params = await algodClient.getTransactionParams().do();
  debug(`fillTxn: got params`);
  return params;
};
void(getTxnParams); // XXX

// const fillTxn = async (round_width, txn) => {
//   return fillTxnWithParams(false, round_width, await getTxnParams(), txn);
// };

// XXX s/number/BigNumber
export const transfer = async (from: Account, to: Account, value: number): Promise<TxnInfo> => {
  const sender = from.networkAccount;
  const receiver = to.networkAccount.addr;

  const params = await algodClient.getTransactionParams().do();
  const note = algosdk.encodeObj('@reach-sh/ALGO.mjs transfer');
  const txn = algosdk.makePaymentTxnWithSuggestedParams(
    sender.addr, receiver, value, undefined, note, params,
  );
  const signedTxn = txn.signTxn(sender.sk);
  const res = await sendAndConfirm(signedTxn, txn);

  if (!res) {
    const txID = txn.txID().toString();
    log_pretty({ txID, from, to, value });
    throw Error(`Transfer failed: ${from} ${to} ${value}`);
  }
  return res;
};

export const connectAccount = async (networkAccount: NetworkAccount) => {
  const thisAcc = networkAccount;
  const shad = thisAcc.addr.substring(2, 6);
  debug(`${shad}: connectAccount`);

  // XXX
  const attach = async (bin: null, ctc: null): Promise<null> => {
    void(bin);
    void(ctc);
    return null
  }
//   const attach = async (bin, ctc) => {
//     debug(`${shad}: attach ${ctc.address}`);

//     let prevRound = ctc.creationRound;
//     debug(`${shad}: attach created at ${prevRound}`);

//     const returnFromTxn = async (txn, evt_cnt) => {
//       const ok_val = txn.ApplicationArgs[1];
//       const ok_args = [];
//       const len = txn.ApplicationArgs.length;
//       for (let i = 0; i < evt_cnt; i++) {
//         ok_args[evt_cnt - 1 - i] = txn.ApplicationArgs[len - i];
//       }
//       const confirmedRound = txn.round;
//       const ok_bal = await getBalanceAt(ctc.address, confirmedRound);
//       prevRound = confirmedRound;
//       return { didTimeout: false, data: ok_args, value: ok_val, balance: ok_bal, from: txn.from };
//     };

//     const iam = (some_addr) => {
//       if (some_addr == thisAcc.addr) {
//         return thisAcc.addr;
//       } else {
//         throw Error(`I should be ${some_addr}, but am ${thisAcc.addr}`);
//       }
//     };

//     const wait = (delta) => {
//       void(delta);
//       throw Error(`XXX Not implemented: wait`);
//     };

//     const sendrecv = async (label, okNum, evt_cnt, tys, args, value, timeout_delay, timeNum, try_p) => {
//       void(tys); // XXX
//       debug(`${shad}: ${label} sendrecv ${okNum} ${timeout_delay} --- START`);

//       const this_is_a_timeout = timeout_delay ? false : true;
//       if (this_is_a_timeout) {
//         timeout_delay = default_range_width;
//       }

//       do {
//         const params = await getTxnParams();
//         const appTxn = await fillTxnWithParams(
//           prevRound, timeout_delay, params, {
//             'from': thisAcc.addr
//               // FIXME JS SDK doesn't handle encoding this kind of txn
//               ,
//             'type': 'appl',
//             'ApplicationId': ctc.appId,
//             'OnCompletion': 'noOp',
//             'ApplicationArgs': [okNum, prevRound, value, ...args],
//             'Accounts': [ctc.address],
//           });
//         const valTxn = await fillTxnWithParams(
//           prevRound, timeout_delay, params, {
//             'type': 'pay',
//             'from': thisAcc.addr,
//             'to': ctc.address,
//             'amount': value,
//           });

//         const otherTxns = [];
//         const txn_out = async (to, amount) => {
//           otherTxns.push(await fillTxnWithParams(
//             prevRound, timeout_delay, params, {
//               'type': 'pay',
//               'from': ctc.address,
//               'to': to,
//               'amount': amount,
//             }));
//         };
//         const fake_txn_res = { didTimeout: false, data: args, value: value, balance: (await getBalanceAt(ctc.address, prevRound)), from: thisAcc.addr };
//         try_p(txn_out, fake_txn_res);

//         const txns = [appTxn, valTxn, ...otherTxns];
//         const txnGroup = algosdk.assignGroupID(txns);
//         void(txnGroup);
//         const signedTxns = [
//           algosdk.signTransaction(appTxn, thisAcc.sk), algosdk.signTransaction(valTxn, thisAcc.sk), ...otherTxns.map(
//             txn =>
//             // FIXME relies on https://github.com/algorand/go-algorand/issues/1051 fix
//             algosdk.signLogicSigTransaction(
//               txn, ctc.logic_sig, [])),
//         ];

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
//     };

//     const recv = async (label, okNum, ok_cnt, timeout_delay, timeout_me, timeout_args, timeNum, try_p) => {
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
//     };

//     return { ...ctc, sendrecv, recv, iam, wait };
//   };

  // XXX
  const deploy = async (bin: null): Promise<null> => {
    void(bin);
    return null;
  };
//   const deploy = async (bin) => {
//     debug(`${shad}: deploy`);

//     const { LogicSigProgram, ApprovalProgram, ClearStateProgram } = bin._Connectors.ALGO;

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

// XXX s/number/BigNumber
export const newTestAccount = async (startingBalance: number) => {
  const networkAccount = algosdk.generateAccount();
  await showBalance('before', networkAccount);
  await transfer({networkAccount: FAUCET}, {networkAccount}, startingBalance);
  await showBalance('after', networkAccount);
  return await connectAccount(networkAccount);
};

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'algos';
/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'microAlgos';

// XXX return BigNumber
/**
 * @description  Parse currency by network
 * @param cm  a currency map, keyed by network, values are the standard unit for that network.
 *   For stdlib/ALGO, this map must include ALGO. The unit is algos.
 * @returns  the amount in the atomic unit of the network.
 *   For stdlib/ALGO this is microAlgos.
 * @example  parseCurrency({ALGO: 100}).toString() // => '100000000'
 */
export function parseCurrency(cm: CurrencyMap): number {
  if (!cm.ALGO) { throw Error(`Expected ALGO in ${Object.keys(cm)}`); }
  const amt =
    isBigNumber(cm.ALGO) ? cm.ALGO.toNumber()
    : typeof cm.ALGO === 'string' ? parseFloat(cm.ALGO)
    : cm.ALGO;
  return algosdk.algosToMicroalgos(amt);
}

// XXX amt: BigNumber
/**
 * @description  Format currency by network
 * @param amt  the amount in the atomic unit of the network.
 *   For stdlib/ALGO this is microAlgos.
 * @param decimals  up to how many decimal places to display in the standard unit.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   For stdlib/ALGO this must be an int from 0 to 9 inclusive, and defaults to 9.
 * @returns  a string representation of that amount in the standard unit for that network.
 *   For stdlib/ALGO this is algos.
 * @example  formatCurrency(100000000); // => '100'
 */
export function formatCurrency(amt: number, decimals: number = 6): string {
  // Recall that 1 algo = 10^6 microalgos
  if (!(Number.isInteger(decimals) && 0 <= decimals && decimals <= 6)) {
    throw Error(`Expected decimals to be an integer from 0 to 6, but got ${decimals}.`);
  }

  // Use decimals+1 and then slice it off to truncate instead of round
  const algosStr = algosdk.microalgosToAlgos(amt).toFixed(decimals+1);
  // Have to roundtrip thru Number to drop trailing zeroes
  return Number(algosStr.slice(0, algosStr.length - 1)).toString();
}

export const newAccountFromMnemonic = false; // XXX
export const getNetworkTime = getLastRound;
export const waitUntilTime = false; // XXX
export const wait = false; // XXX
export const verifyContract = false; // XXX
