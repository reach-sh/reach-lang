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
  return await waitForConfirmation(tx.txID, untilRound); }

const sendAndConfirm = async (txn, untilRound) => {
  await algodClient.sendRawTransaction(txn.blob);
  return await waitForConfirmation(txn.txID, untilRound); }

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

    const sendrecv = async (label, funcNum, args, value, timeout_delay, timeout_evt, try_p) => {
      debug(`${shad}: ${label} sendrecv ${funcNum} ${timeout_delay} --- START`);

      const params = await getTxnParams();
      const valTxn = await fillTxnWithParams(
        prevRound, timeout_delay, params, {
          "type": "pay"
          , "from": thisAcc.addr
          , "to": ctc.address
          , "amount": value
        } );
      const appTxn = await fillTxnWithParams(
        prevRound, timeout_delay, params, {
          "from": thisAcc.addr
          , "type": "appl"
          , "ApplicationId": ctc.appId
          , "OnCompletion": "noOp"
          , "ApplicationArgs": [funcNum, prevRound, ...args]
          // , "Accounts": 0
          // , "ForeignApps": 0
          // , "ApprovalProgram": 0
          // , "ClearStateProgram": 0
          // , "LocalStateSchema": 0
          // , "GlobalStateSchema": 0
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
      // FIXME This 42 is weird.
      const fake_txn_res = { didTimeout: false, data: args, value: value, balance: 42, from: thisAcc.addr };
      try_p( txn_out, fake_txn_res );
      
      const txns = [ appTxn, valTxn, ...otherTxns ];
      const txnGroup = algosdk.assignGroupID(txns);
      const signedTxns = [
        algosdk.signTransaction(appTxn, thisAcc.sk)
        , algosdk.signTransaction(valTxn, thisAcc.sk)
        , ...otherTxns.map(txn => algosdk.signLogicSigTransaction( txn, ctc.logic_sig )) ];

      const confirmedTxn = await sendsAndConfirm( signedTxns, appTxn.lastRound );
      if ( confirmedTxn ) {
        const confirmedRound = confirmedTxn.round;
        debug(`${shad}: ${label} send ${funcNum} ${timeout_delay} --- OKAY`);
        // FIXME: Should be confirmedRound balance, but this requires "the next indexer version" (Max on 2020/05/05)
        const ok_bal = (await algodClient.accountInformation(ctc.address)).amount;
        prevRound = confirmedRound;
        return { didTimeout: false, data: args, value: value, balance: ok_bal, from: thisAcc.addr }; }

      debug(`${shad}: ${label} send ${funcNum} ${timeout_delay} --- FAIL/TIMEOUT`);
      const rec_res = await recv(label, timeout_evt, false, false, false, false, false);
      rec_res.didTimeout = true;
      return rec_res; };

    const recv = async (label, ok_num, timeout_delay, timeout_me, timeout_args, timeout_num, try_p) => {
      debug(`${shad}: ${label} recv ${ok_num} ${timeout_delay} --- START`);

      // XXX look through these for a transaction to us
      algodClient.transactionByAddress(ctc.address, prevRound, prevRound + timeout_delay);

      // XXX have to figure out the actual round it was in
      prevRound = confirmedRound;

      debug(`XXX recv`); };

    return { sendrecv, recv, address: thisAcc.addr }; };

  const deploy = async (bin) => {
    debug(`${shad}: deploy`);

    const { LogicSigProgram, ApprovalProgram, ClearStateProgram } = bin.ALGO;

    debug(`${shad}: deploy: making account`);
    const ctc_acc = algosdk.generateAccount();
    // FIXME current JS SDK rejects our version
    const logic_sig = algosdk.makeLogicSig(Buffer.from(LogicSigProgram, "base64"));
    logic_sig.sign( ctc_acc.sk );

    debug(`${shad}: deploy: filling transaction`);
    const appTxn = await fillTxn( default_range_width, {
      "from": thisAcc.addr
      , "type": "appl"
      , "ApplicationId": 0
      , "OnCompletion": "noOp"
      , "ApplicationArgs": [ 0, ctc_acc.addr ]
      , "ApprovalProgram": ApprovalProgramB64
      , "ClearStateProgram": ClearStateProgramB64
      , "GlobalStateSchema": { "NumByteSlice": 2 }
      // FIXME: SDK should allow me to not include these
      // , "LocalStateSchema": 0
      // , "Accounts": 0
      // , "ForeignApps": 0
    } );
    debug(`${shad}: deploy: signing transction`);
    const signedTxn = algosdk.signTransaction(appTxn, thisAcc.sk);
    debug(`${shad}: deploy: txn signed, sending`);
    const confirmedTxn = await sendAndConfirm(signedTxn);
    debug(`${shad}: deploy: application confirmed`);
    const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
    const creationRound = confirmTxn.round;

    const ctc = { address: ctc_acc.addr, appId, creationRound, logic_sig };

    return attach(bin, ctc); };

  return { deploy, attach, address: thisAcc.addr }; };

const showBalance = async (note, acc) => {
  let theInfo = await algodClient.accountInformation(acc.addr);
  console.log("%s: balance: %d microAlgos", note, theInfo.amount) };

export const newTestAccount = async (startingBalance) => {
  const acc = algosdk.generateAccount();
  showBalance("before", acc);
  await transfer(acc, FAUCET, startingBalance);
  showBalance("after", acc);
  return connectAccount(acc); };
