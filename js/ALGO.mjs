import algosdk from 'algosdk';

import { debug } from './shared.mjs';
export * from './shared.mjs';

export const {
  algosToMicroalgos,
  microalgosToAlgos,
} = algosdk;

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict


// networkAccount[ALGO] = {
//   addr: string         // Address
//   sk: Uint8Array(64)   // Secret Key
// }

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

const log_pretty = (obj) =>
  console.log(JSON.parse(JSON.stringify(obj)));

// Common interface exports

// TODO: read token from scripts/algorand-devnet/algorand_data/algod.token
const token = process.env.ALGO_TOKEN || 'c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705';
const server = process.env.ALGO_SERVER || 'http://localhost';
const port = process.env.ALGO_PORT || 4180;
const algodClient = new algosdk.Algodv2(token, server, port);

const default_range_width = 1000;

// eslint-disable-next-line max-len
const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || 'pulp abstract olive name enjoy trick float comfort verb danger eternal laptop acquire fetch message marble jump level spirit during benefit sure dry absent history'));
// if using the default:
// assert(FAUCET.addr === 'EYTSJVJIMJDUSRRNTMVLORTLTOVDWZ6SWOSY77JHPDWSD7K3P53IB3GUPQ');


// Helpers

const getLastRound = async () =>
  (await algodClient.status().do())['last-round'];

const waitForConfirmation = async (txId, untilRound) => {
  let lastRound = null;
  do {
    const lastRoundAfterCall = lastRound ?
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
  return false;
};

// XXX Untested but something like this should work
// [{signedTxn, txn}] -> ()
const sendsAndConfirm = async (txns) => {
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

const sendAndConfirm = async (signedTxn, txn) => {
  const txID = txn.txID().toString();
  const untilRound = txn.lastRound;
  await algodClient.sendRawTransaction(signedTxn).do();
  return await waitForConfirmation(txID, untilRound);
};

// Backend
const fillTxnWithParams = (firstRound, round_width, params, txn0) => {
  // Don't need these anymore?
  void(round_width);
  void(firstRound);
  // const theFirstRound = firstRound ? firstRound : params.lastRound;
  // FIXME these fields don't match the documentation https://developer.algorand.org/docs/reference/transactions/ so update once they fix this issue https://github.com/algorand/js-algorand-sdk/issues/144
  // Fixed?
  // txn.flatFee = params.flatFee;
  // txn.fee = params.fee;
  // txn.firstRound = theFirstRound;
  // txn.lastRound = theFirstRound + round_width;
  // txn.genesisID = params.genesisID;
  // txn.genesisHash = params.genesisHash;
  return {
    ...params,
    ...txn0,
  };
};

const getTxnParams = async () => {
  debug(`fillTxn: getting params`);
  const params = await algodClient.getTransactionParams().do();
  debug(`fillTxn: got params`);
  return params;
};

const fillTxn = async (round_width, txn) => {
  return fillTxnWithParams(false, round_width, await getTxnParams(), txn);
};

export const transfer = async (from, to, value) => {
  if (from.networkAccount) return await transfer(from.networkAccount, to, value);
  if (to.networkAccount) return await transfer(from, to.networkAccount, value);
  if (to.addr) return await transfer(from, to.addr, value);

  const params = await algodClient.getTransactionParams().do();
  const note = algosdk.encodeObj('@reach-sh/ALGO.mjs transfer');
  const txnArgs = [from.addr, to, value, undefined, note, params];
  const txn = algosdk.makePaymentTxnWithSuggestedParams(...txnArgs);
  const signedTxn = txn.signTxn(from.sk);
  const res = await sendAndConfirm(signedTxn, txn);

  if (!res) {
    const txID = txn.txID().toString();
    log_pretty({ txID, from, to, value });
    throw Error(`Transfer failed: ${from} ${to} ${value}`);
  }
  return res;
};

export const connectAccount = async thisAcc => {
  const shad = thisAcc.addr.substring(2, 6);
  debug(`${shad}: connectAccount`);

  const attach = async (bin, ctc) => {
    debug(`${shad}: attach ${ctc.address}`);

    let prevRound = ctc.creationRound;
    debug(`${shad}: attach created at ${prevRound}`);

    const returnFromTxn = async (txn, evt_cnt) => {
      const ok_val = txn.ApplicationArgs[1];
      const ok_args = [];
      const len = txn.ApplicationArgs.length;
      for (let i = 0; i < evt_cnt; i++) {
        ok_args[evt_cnt - 1 - i] = txn.ApplicationArgs[len - i];
      }
      const confirmedRound = txn.round;
      const ok_bal = await getBalanceAt(ctc.address, confirmedRound);
      prevRound = confirmedRound;
      return { didTimeout: false, data: ok_args, value: ok_val, balance: ok_bal, from: txn.from };
    };

    const iam = (some_addr) => {
      if (some_addr == thisAcc.addr) {
        return thisAcc.addr;
      } else {
        throw Error(`I should be ${some_addr}, but am ${thisAcc.addr}`);
      }
    };

    const wait = (delta) => {
      void(delta);
      throw Error(`XXX Not implemented: wait`);
    };

    const sendrecv = async (label, okNum, evt_cnt, tys, args, value, timeout_delay, timeNum, try_p) => {
      void(tys); // XXX
      debug(`${shad}: ${label} sendrecv ${okNum} ${timeout_delay} --- START`);

      const this_is_a_timeout = timeout_delay ? false : true;
      if (this_is_a_timeout) {
        timeout_delay = default_range_width;
      }

      do {
        const params = await getTxnParams();
        const appTxn = await fillTxnWithParams(
          prevRound, timeout_delay, params, {
            'from': thisAcc.addr
              // FIXME JS SDK doesn't handle encoding this kind of txn
              ,
            'type': 'appl',
            'ApplicationId': ctc.appId,
            'OnCompletion': 'noOp',
            'ApplicationArgs': [okNum, prevRound, value, ...args],
            'Accounts': [ctc.address],
          });
        const valTxn = await fillTxnWithParams(
          prevRound, timeout_delay, params, {
            'type': 'pay',
            'from': thisAcc.addr,
            'to': ctc.address,
            'amount': value,
          });

        const otherTxns = [];
        const txn_out = async (to, amount) => {
          otherTxns.push(await fillTxnWithParams(
            prevRound, timeout_delay, params, {
              'type': 'pay',
              'from': ctc.address,
              'to': to,
              'amount': amount,
            }));
        };
        const fake_txn_res = { didTimeout: false, data: args, value: value, balance: (await getBalanceAt(ctc.address, prevRound)), from: thisAcc.addr };
        try_p(txn_out, fake_txn_res);

        const txns = [appTxn, valTxn, ...otherTxns];
        const txnGroup = algosdk.assignGroupID(txns);
        void(txnGroup);
        const signedTxns = [
          algosdk.signTransaction(appTxn, thisAcc.sk), algosdk.signTransaction(valTxn, thisAcc.sk), ...otherTxns.map(
            txn =>
            // FIXME relies on https://github.com/algorand/go-algorand/issues/1051 fix
            algosdk.signLogicSigTransaction(
              txn, ctc.logic_sig, [])),
        ];

        const confirmedTxn = await sendsAndConfirm(signedTxns, appTxn.lastRound);
        if (confirmedTxn) {
          debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- OKAY`);
          // FIXME no documentation on whether confirmedTxn has something for each txn in a group or only one thing. We made need to call returnFromTxn with appTxn for the data and confirmedTxn for the round/etc
          return (await returnFromTxn(confirmedTxn, evt_cnt));
        }

        if (!this_is_a_timeout) {
          debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- FAIL/TIMEOUT`);
          const rec_res = await recv(label, timeNum, 0, false, false, false, false, false);
          rec_res.didTimeout = true;
          return rec_res;
        }
      }
      while (this_is_a_timeout);
    };

    const recv = async (label, okNum, ok_cnt, timeout_delay, timeout_me, timeout_args, timeNum, try_p) => {
      debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- START`);

      const this_is_a_timeout = timeout_delay ? false : true;
      if (this_is_a_timeout) {
        timeout_delay = default_range_width;
      }

      while (1) {
        const startRound = this_is_a_timeout ? prevRound : await getLastRound();
        const untilRound = prevRound + timeout_delay;
        while ((await getLastRound()) < untilRound) {
          // FIXME maxj says there will be a better query api in the future, when that is in place, push this forEach to the indexer
          const resp = await algodClient.transactionByAddress(ctc.address, startRound, untilRound).do();
          resp.transactions.forEach(async txn => {
            if (txn.type == 'appl' &&
              txn.ApplicationId == ctc.appId &&
              txn.ApplicationArgs[0] == okNum &&
              txn.ApplicationArgs[1] == prevRound) {
              debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- OKAY`);
              return (await returnFromTxn(txn, ok_cnt));
            }
          });
        }

        if (!this_is_a_timeout) {
          debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- TIMEOUT`);
          const rec_res = timeout_me ?
            await sendrecv(label, timeNum, 0, timeout_args, 0, false, false, try_p) :
            await recv(label, timeNum, 0, false, false, false, false, false);
          rec_res.didTimeout = true;
          return rec_res;
        }
      }
    };

    return { ...ctc, sendrecv, recv, iam, wait };
  };

  const deploy = async (bin) => {
    debug(`${shad}: deploy`);

    const { LogicSigProgram, ApprovalProgram, ClearStateProgram } = bin._Connectors.ALGO;

    debug(`${shad}: deploy: making account`);
    const ctc_acc = algosdk.generateAccount();

    debug(`${shad}: deploy: filling transaction`);
    const appTxn = await fillTxn(default_range_width, {
      // FIXME JS SDK doesn't handle this kind of txn
      'from': thisAcc.addr,
      'type': 'appl',
      'ApplicationId': 0,
      'OnCompletion': 'noOp',
      'ApplicationArgs': [],
      'ApprovalProgram': ApprovalProgram,
      'ClearStateProgram': ClearStateProgram,
      'GlobalStateSchema': { 'NumByteSlice': 2 },
      'Accounts': [ctc_acc.addr],
      // FIXME: Use note field for link to Reach code
    });
    debug(`${shad}: deploy: signing transction`);
    const signedTxn = algosdk.signTransaction(appTxn, thisAcc.sk);
    debug(`${shad}: deploy: txn signed, sending`);
    const confirmedTxn = await sendAndConfirm(signedTxn);
    debug(`${shad}: deploy: application confirmed`);
    const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
    const creationRound = confirmedTxn.round;

    // FIXME relies on https://github.com/algorand/go-algorand/issues/1051 fix
    const logic_sig = algosdk.makeLogicSig(Buffer.from(LogicSigProgram, 'base64'), [appId]);
    logic_sig.sign(ctc_acc.sk);

    const ctc = { address: ctc_acc.addr, appId, creationRound, logic_sig };

    return attach(bin, ctc);
  };

  return { deploy, attach, networkAccount: thisAcc };
};

const getBalanceAt = async (addr, round) => {
  void(round);
  // FIXME: Don't ignore round, but this requires 'the next indexer version' (Max on 2020/05/05)
  return (await algodClient.accountInformation(addr).do()).amount;
};

export const balanceOf = async acc => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  return (await getBalanceAt(networkAccount.addr, await getLastRound()));
};

const showBalance = async (note, networkAccount) => {
  const bal = await balanceOf({ networkAccount });
  const showBal = algosdk.microalgosToAlgos(bal).toFixed(2);
  console.log('%s: balance: %s algos', note, showBal);
};

export const newTestAccount = async (startingBalance) => {
  const acc = algosdk.generateAccount();
  await showBalance('before', acc);
  await transfer(FAUCET, acc, startingBalance);
  await showBalance('after', acc);
  return await connectAccount(acc);
};

export const newAccountFromMnemonic = false; // XXX

export const getNetworkTime = getLastRound;
export const waitUntilTime = false; // XXX
export const wait = false; // XXX
export const verifyContract = false; // XXX
