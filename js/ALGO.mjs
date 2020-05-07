import algosdk from 'algosdk';

const token = process.env.ALGO_TOKEN || "88f05de47936c12a756a28012aba3a24c3d06c45acd27fdcadb2a08ba32dc658";
const server = process.env.ALGO_SERVER || "http://localhost";
const port = process.env.ALGO_PORT || 8080;
const algodClient = new algosdk.Algod(token, server, port);

const default_range_width = 1000;

const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || "truck lobster turn fish foot paper select enough basket scout rack swallow chuckle laugh lava trumpet evidence pottery range news satoshi want popular absent math"));

// Helpers
const DEBUG = true;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG: ${msg}`); } };

const panic = e => { throw Error(e); };

const waitForConfirmation = async (txId, untilRound) => {
  while (true) {
    const lastRound = (await algodClient.status()).lastRound;
    if ( lastRound > untilRound ) {
      return false; }
    const pendingInfo =
          await algodClient.pendingTransactionInformation(txId);
    if (pendingInfo.round != null && pendingInfo.round > 0) {
      return pendingInfo; }
    await algodClient.statusAfterBlock(lastRound + 1); } };

const sendsAndConfirm = async (txns, untilRound) => {
  const btxns = txns.map(o => o.blob);
  const tx = await algodClient.sendRawTransactions(btxns);
  // FIXME https://developer.algorand.org/docs/features/atomic_transfers/ ... Send transactions ... claims that tx has a txId field, but it doesn't as far as I can tell, because this crashes.
  return (await waitForConfirmation(tx.txID, untilRound)); }

const sendAndConfirm = async (txn, untilRound) => {
  await algodClient.sendRawTransaction(txn.blob);
  return (await waitForConfirmation(txn.txID, untilRound)); }

// Client API
export const assert = d => nodeAssert.strict(d);

// Backend
const fillTxnWithParams = ( firstRound, round_width, params, txn ) => {
  const theFirstRound = firstRound ? firstRound : params.lastRound;
  txn.fee = params.minFee;
  txn.firstRound = theFirstRound;
  txn.lastRound = theFirstRound + round_width;
  txn.genesisID = params.genesisID;
  txn.genesisHash = params.genesishashb64;
  return txn; };

const getTxnParams = async () => {
  debug(`fillTxn: getting params`);
  const params = await algodClient.getTransactionParams();
  debug(`fillTxn: got params`);
  return params; }

const fillTxn = async ( round_width, txn ) => {
  return fillTxnWithParams( false, round_width, await getTxnParams(), txn ); };

export const transfer = async (to, from, value) => {
  const txn = await fillTxn( default_range_width, {
    "type": "pay",
    "from": from.addr,
    "to": to.addr,
    "amount": value } );
  const signedTxn = algosdk.signTransaction(txn, from.sk);
  const res = await sendAndConfirm( signedTxn, txn.lastRound );
  if ( ! res ) {
    panic(`Transfer failed: ${to} ${from} ${value}`); }
  return res; };

export const connectAccount = async thisAcc => {
  const shad = thisAcc.addr.substring(2,6);
  debug(`${shad}: connectAccount`);

  const attach = async (bin, ctc) => {
    debug(`${shad}: attach ${ctc.address}`);

    let prevRound = ctc.creationRound;
    debug(`${shad}: attach created at ${prevRound}`);

    const returnFromTxn = async (txn, evt_cnt) => {
      const ok_val = txn.ApplicationArgs[1];
      const ok_args = [];
      const len = txn.ApplicationArgs.length;
      for ( const i = 0; i < evt_cnt; i++ ) {
        ok_args[evt_cnt - 1 - i] = txn.ApplicationArgs[len - i]; }
      const ok_bal = await getBalanceAt(ctc.address, confirmedRound);
      prevRound = txn.round;
      return { didTimeout: false, data: ok_vals, value: ok_val, balance: ok_bal, from: txn.from }; };
    
    const sendrecv = async (label, okNum, evt_cnt, args, value, timeout_delay, timeNum, try_p) => {
      debug(`${shad}: ${label} sendrecv ${okNum} ${timeout_delay} --- START`);

      // XXX What if timeout_delay is false
      
      const params = await getTxnParams();
      const appTxn = await fillTxnWithParams(
        prevRound, timeout_delay, params, {
          "from": thisAcc.addr
          , "type": "appl"
          , "ApplicationId": ctc.appId
          , "OnCompletion": "noOp"
          , "ApplicationArgs": [okNum, prevRound, value, ...args]
          , "Accounts" : [ ctc_acc.addr ]
        } );
      const valTxn = await fillTxnWithParams(
        prevRound, timeout_delay, params, {
          "type": "pay"
          , "from": thisAcc.addr
          , "to": ctc.address
          , "amount": value
        } );

      const otherTxns = [];
      const txn_out = async ( to, amount ) => {
        otherTxns.push( await fillTxnWithParams(
        prevRound, timeout_delay, params, {
          "type": "pay"
          , "from": ctc.address
          , "to": to
          , "amount": amount
        } ) );
      };
      const fake_txn_res = { didTimeout: false, data: args, value: value, balance: (await getBalanceAt(ctc.address, prevRound)), from: thisAcc.addr };
      try_p( txn_out, fake_txn_res );
      
      const txns = [ appTxn, valTxn, ...otherTxns ];
      const txnGroup = algosdk.assignGroupID(txns);
      const signedTxns = [
        algosdk.signTransaction(appTxn, thisAcc.sk)
        , algosdk.signTransaction(valTxn, thisAcc.sk)
        , ...otherTxns.map(txn => algosdk.signLogicSigTransaction( txn, ctc.logic_sig, [] )) ];

      const confirmedTxn = await sendsAndConfirm( signedTxns, appTxn.lastRound );
      if ( confirmedTxn ) {
        debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- OKAY`);
        return (await returnFromTxn( confirmedTxn, evt_cnt )); }

      debug(`${shad}: ${label} send ${okNum} ${timeout_delay} --- FAIL/TIMEOUT`);
      const rec_res = await recv(label, timeNum, 0, false, false, false, false, false);
      rec_res.didTimeout = true;
      return rec_res; };

    const recv = async (label, okNum, ok_cnt, timeout_delay, timeout_me, timeout_args, timeNum, try_p) => {
      debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- START`);

      // XXX What if timeout_delay is false

      const untilRound = prevRound + timeout_delay;
      while ( (await algodClient.status()).lastRound < untilRound ) {
        const resp = await algodClient.transactionByAddress(ctc.address, prevRound, untilRound);
        resp.transactions.forEach(async txn => {
          if ( txn.type == "appl"
               && txn.ApplicationId == ctc.appId
               && txn.ApplicationArgs[0] == okNum
               && txn.ApplicationArgs[1] == prevRound ) {
            debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- OKAY`);
            return (await returnFromTxn( txn, evt_cnt )); } }); }

      debug(`${shad}: ${label} recv ${okNum} ${timeout_delay} --- TIMEOUT`);
      const rec_res = timeout_me
            ? await sendrecv(label, timeNum, 0, timeout_args, 0, false, false, ((a, b) => { return; }) )
            : await recv(label, timeNum, 0, false, false, false, false, false);
      rec_res.didTimeout = true;
      return rec_res; };

    return { sendrecv, recv, address: thisAcc.addr }; };

  const deploy = async (bin) => {
    debug(`${shad}: deploy`);

    const { LogicSigProgram, ApprovalProgram, ClearStateProgram } = bin.ALGO;

    debug(`${shad}: deploy: making account`);
    const ctc_acc = algosdk.generateAccount();

    debug(`${shad}: deploy: filling transaction`);
    const appTxn = await fillTxn( default_range_width, {
      "from": thisAcc.addr
      , "type": "appl"
      , "ApplicationId": 0
      , "OnCompletion": "noOp"
      , "ApplicationArgs": []
      , "ApprovalProgram": ApprovalProgram
      , "ClearStateProgram": ClearStateProgram
      , "GlobalStateSchema": { "NumByteSlice": 2 }
      , "Accounts" : [ ctc_acc.addr ]
      // FIXME: Use note field for link to Reach code
    } );
    debug(`${shad}: deploy: signing transction`);
    const signedTxn = algosdk.signTransaction(appTxn, thisAcc.sk);
    debug(`${shad}: deploy: txn signed, sending`);
    const confirmedTxn = await sendAndConfirm(signedTxn);
    debug(`${shad}: deploy: application confirmed`);
    const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
    const creationRound = confirmTxn.round;

    const logic_sig = algosdk.makeLogicSig(Buffer.from(LogicSigProgram, "base64"), [ appId ]);
    logic_sig.sign( ctc_acc.sk );
    
    const ctc = { address: ctc_acc.addr, appId, creationRound, logic_sig };

    return attach(bin, ctc); };

  return { deploy, attach, address: thisAcc.addr }; };

const getBalanceAt = async (addr, round) => {
  // FIXME: Don't ignore round, but this requires "the next indexer version" (Max on 2020/05/05)
  return (await algodClient.accountInformation(addr)).amount; }

const showBalance = async (note, acc) => {
  let theInfo = await algodClient.accountInformation(acc.addr);
  console.log("%s: balance: %d microAlgos", note, theInfo.amount) };

export const newTestAccount = async (startingBalance) => {
  const acc = algosdk.generateAccount();
  showBalance("before", acc);
  await transfer(acc, FAUCET, startingBalance);
  showBalance("after", acc);
  return connectAccount(acc); };
