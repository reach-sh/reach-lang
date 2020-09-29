// XXX: do not import any types from algosdk; instead copy/paste them below
// XXX: can stop doing this workaround once @types/algosdk is shippable
import algosdk from 'algosdk';
import base32 from 'hi-base32';
import ethers, { BigNumber } from 'ethers';
import Timeout from 'await-timeout';

import {
  CurrencyAmount, debug,
  isBigNumber, bigNumberify, bigNumberToHex,
  T_UInt256, T_Bool,
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
type SignedTxn = Uint8Array;
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
type LogicArg = Uint8Array | string;

type CompileResultBytes = {
  src: String,
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

type Recv = {
  didTimeout: false,
  data: Array<ContractOut>,
  value: BigNumber,
  balance: BigNumber,
  from: Address,
} | { didTimeout: true };

type ContractAttached = {
  getInfo: () => Promise<ContractInfo>,
  sendrecv: (...argz: any) => Promise<Recv>,
  recv: (...argz: any) => Promise<Recv>,
  wait: (...argz: any) => any,
  iam: (some_addr: Address) => Address,
};

// TODO
type ContractOut = any;

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

const itoken = process.env.ALGO_INDEXER_TOKEN || 'reach-devnet';
const iserver = process.env.ALGO_INDEXER_SERVER || 'http://localhost';
const iport = process.env.ALGO_INDEXER_PORT || 8980;
const indexer = new algosdk.Indexer(itoken, iserver, iport);

// eslint-disable-next-line max-len
const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || 'pulp abstract olive name enjoy trick float comfort verb danger eternal laptop acquire fetch message marble jump level spirit during benefit sure dry absent history'));
// if using the default:
// assert(FAUCET.addr === 'EYTSJVJIMJDUSRRNTMVLORTLTOVDWZ6SWOSY77JHPDWSD7K3P53IB3GUPQ');

// Helpers

const getLastRound = async (): Promise<Round> =>
  (await algodClient.status().do())['last-round'];

const waitForConfirmation = async (txId: TxId, untilRound: number): Promise<TxnInfo> => {
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

const sendAndConfirm = async (
  stx_or_stxs: SignedTxn | Array<SignedTxn>, txn: Txn
): Promise<TxnInfo> => {
  const txID = txn.txID().toString();
  const untilRound = txn.lastRound;
  const req = algodClient.sendRawTransaction(stx_or_stxs);
  // @ts-ignore XXX
  debug(`sendAndConfirm: ${base64ify(req.txnBytesToPost)}`);
  try {
    await req.do();
  } catch (e) {
    throw { type: 'sendRawTransaction', e };
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
    const params = await algodClient.getTransactionParams().do();
    debug(`fillTxn: got params: ${JSON.stringify(params)}`);
    if (params.firstRound !== 0) {
      return params;
    }
    debug(`...but firstRound is 0, so let's wait and try again.`);
    // Assumption: firstRound will move past 0 on its own.
    await Timeout.set(1);
  }
};

const sign_and_send_sync = async (
  label: string,
  sk: SecretKey,
  txn: Txn,
): Promise<TxnInfo> => {
  const txn_s = txn.signTxn(sk);
  try {
    return await sendAndConfirm(txn_s, txn);
  } catch (e) {
    throw Error(`${label} txn failed:\n${JSON.stringify(txn)}\nwith:\n${JSON.stringify(e)}`);
  }
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
    `transfer ${JSON.stringify(from)} ${JSON.stringify(to)} ${valuen}`,
    sender.sk,
    algosdk.makePaymentTxnWithSuggestedParams(
      sender.addr, receiver, valuen, undefined, note, await getTxnParams(),
    ));
};

// XXX I'd use replaceAll if I could, but I'm pretty sure there's just one occurrence. It would be better to extend ConnectorInfo so these are functions
const replaceAddr = (label: string, addr: Address, x:string): string =>
x.replace(`"{{${label}}}"`,
          `base32(${base32.encode(algosdk.decodeAddress(addr).publicKey).toString()})`);

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

// XXX I'm using this to inspect the msgpack struct, but maybe just do a round-trip through encode/decode and see what I get?
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
      sim_p: (fake: Recv) => SimRes,
    ): Promise<Recv> => {
      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} sendrecv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);
      const handler = bin_comp.steps[funcName];

      const fake_res = {
        didTimeout: false,
        data: args,
        value: value,
        balance: bigNumberify('0'), // XXX
        from: thisAcc.addr,
      };
      const sim_r = sim_p( fake_res );
      const isHalt = sim_r.isHalt;
      const sim_txns = sim_r.txns;

      const actual_args =
        [ sim_r.prevSt, sim_r.nextSt, isHalt, lastRound, ...args ];
      const actual_tys =
        [ T_UInt256, T_UInt256, T_Bool, T_UInt256, ...tys ];
      const munged_args =
        // XXX this needs to be customized for Algorand, so I don't have to safeify. Ideally munge would return Uint8Array for everything.
        actual_args.map((m, i) => actual_tys[i].munge(actual_tys[i].canonicalize(m)));

      const safeify = (x: any): LogicArg => {
        if ( isBigNumber(x) ) {
          // XXX Does it matter that this is not msgpacked as an int?
          const size = x.lt(bigNumberify(2).pow(64)) ? 8 : 32;
          const h = '0x' + bigNumberToHex(x, size);
          debug(`${x} =${size}> ${h}`);
          const r = ethers.utils.arrayify(h);
          return r;
        } else if ( typeof x === 'boolean' ) {
          return safeify(bigNumberify(x ? 1 : 0));
        } else if ( typeof x === 'string' ) {
          return x;
        } else {
          throw Error(`can't safeify ${JSON.stringify(x)}`);
        }
      };
      const safe_args: Array<LogicArg> = munged_args.map(safeify);
      safe_args.forEach((x) => { 
        if (! ( typeof x === 'string' || x instanceof Uint8Array ) ) {
          throw Error(`expect safe program argument, got ${JSON.stringify(x)}`);
        }
      });

      debug(`${dhead} --- PREPARE`); // XXX display safe_args usefully
      const handler_with_args =
        algosdk.makeLogicSig(handler.result, safe_args);
      debug(`${dhead} --- PREPARED`); // XXX display handler_with_args usefully, like with base64ify toBytes

      while ( true ){
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
            txnFromHandler, handler_with_args).blob;
        debug(`txnFromHandler_s: ${base64ify(txnFromHandler_s)}`);
        const txnToHandler_s = sign_me(txnToHandler);
        const txnToContract_s = sign_me(txnToContract);
        const txnFromContracts_s =
          txnFromContracts.map(
            (txn: Txn) => 
            algosdk.signLogicSigTransactionObject(txn, ctc_prog).blob);

        const txns_s = [
          txnAppl_s,
          txnFromHandler_s,
          txnToHandler_s,
          txnToContract_s,
          ...txnFromContracts_s
        ];

        // XXX rather than bothering with this, add an option to algod to producing debugging output when running lsigs
        if ( false && getDEBUG() ) {
          const dr_apps: Array<any> = [
            // XXX
          ];
          const dr_accts: Array<any> = [
            // XXX
          ];
          void(dr_apps);
          void(dr_accts);
          const dr_srcs: Array<any> = [
         //   new algosdk.modelsv2.DryrunSource("approv", bin_comp.appApproval.src, 0),
            new algosdk.modelsv2.DryrunSource("lsig", handler.src, 1)
          ];
          for ( let i = 0; i < sim_txns.length; i++ ) {
            dr_srcs.push(new algosdk.modelsv2.DryrunSource("lsig", bin_comp.ctc.src, 4 + i));
          }
          const dr = new algosdk.modelsv2.DryrunRequest({
            txns: txns_s,
            // accounts: dr_accts,
            // apps: dr_apps,
            sources: dr_srcs });
          try {
            const drr = await algodClient.dryrun(dr).do();
            // XXX parse this, rather than just displaying it
            console.log(`dryrun:\n${JSON.stringify(drr, null, 2)}`);
          } catch (e: any) {
            console.log(`${dhead} --- DRY-RUN:\n${format_failed_request(e)}`);
          }
        }

        debug(`${dhead} --- SEND: ${txns_s.length}`);
        let res;
        try {
          // XXX somewhere in this, txnFromHandler appears to be dropped
          res = await sendAndConfirm( txns_s, txnAppl );
        } catch (e) {
          if ( e.type == "sendRawTransaction" ) {
            // XXX when this fails, it is dropping the lsig txn
            throw Error(`${dhead} --- FAIL:\n${format_failed_request(e.e)}`);
          } else {
            throw Error(`${dhead} --- FAIL:\n${JSON.stringify(e)}`);
          }
        }

        // XXX we should inspect res and if we failed because we didn't get picked out of the queue, then we shouldn't error, but should retry and let the timeout logic happen.
        void(res);
        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      }
    };

    const recv = async (
      label: string,
      funcNum: number, 
      evt_cnt: number,
      tys: Array<any>,
      timeout_delay: undefined | BigNumber
    ): Promise<Recv> => {
      const funcName = `m${funcNum}`;
      const dhead = `${shad}: ${label} recv ${funcName} ${timeout_delay}`;
      debug(`${dhead} --- START`);
      const handler = bin_comp.steps[funcName];

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
          .minRound(lastRound);
        if ( timeoutRound ) {
          query = query.maxRound(timeoutRound);
        }
        debug(`${dhead} --- QUERY = ${JSON.stringify(query)}`);
        let res;
        try {
          res = await query.do();
        } catch (e) {
          throw Error(`${dhead} --- QUERY FAIL: ${JSON.stringify(e)}`);
        }

        if ( res.transactions.length == 0 ) {
          debug(`${dhead} --- RESULT = empty`);
          // XXX Look at the round in res and wait for a new round
          await Timeout.set(1);
          continue;
        }

        debug(`${dhead} --- RESULT = ${JSON.stringify(res)}`);

        void(tys);
        void(evt_cnt);
        // XXX need to parse res

        throw Error(`XXX recv`);
      }
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

export const balanceOf = async (acc: Account): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);
  return bigNumberify(await getBalanceAt(networkAccount.addr, await getLastRound()));
};

const showBalance = async (note: string, networkAccount: NetworkAccount) => {
  const bal = await balanceOf({ networkAccount });
  const showBal = formatCurrency(bal, 2);
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
