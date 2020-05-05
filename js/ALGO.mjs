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

const sendAndConfirm = async (txn, untilRound) => {
  await algodClient.sendRawTransaction(txn.blob)
  return await waitForConfirmation(txn.txID, untilRound); }

// Client API
export const assert = d => nodeAssert.strict(d);

// Backend
const fillTxn = async ( round_width, txn ) => {
  debug(`fillTxn: getting params`);
  const params = await algodClient.getTransactionParams();
  debug(`fillTxn: got params`);
  txn.fee = params.minFee;
  txn.firstRound = params.lastRound;
  txn.lastRound = params.lastRound + round_width;
  txn.genesisID = params.genesisID;
  txn.genesisHash = params.genesishashb64;
  return txn; };

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
    const { LogicSigProgramB64 } = bin.ALGO;
    debug(`${shad}: attach ${ctc.address}`);

    let prevRound = ctc.creationRound;
    debug(`${shad}: attach created at ${prevRound}`);
    
    const sendrecv = async (label, funcNum, args, value, timeout_delay, timeout_evt, try_p) => {
      debug(`${shad}: ${label} sendrecv ${funcNum} ${timeout_delay} --- START`);

      // XXX use value to make value txn
      const appTxn = await fillTxn( timeout_delay, {
        "from": thisAcc.addr,
        "type": "appl",
        "ApplicationId": ctc.appId,
        "OnCompletion": "noOp",
        "ApplicationArgs": algosdk.encodeObj([funcNum, prevRound, ...args]),
        // FIXME: SDK should allow me to not include these
        "Accounts": 0,
        "ForeignApps": 0,
        "ApprovalProgram": 0,
        "ClearStateProgram": 0,
        "LocalStateSchema": 0,
        "GlobalStateSchema": 0
      } );
      // XXX use try_p to figure out transfers in this

      const send_res = await sendAndConfirm( signedTxn, appTxn.lastRound );
      if ( send_res ) {
        const confirmedRound = confirmTxn.round;
        debug(`${shad}: ${label} send ${funcNum} ${timeout_delay} --- OKAY`);
        // FIXME: Should be confirmedRound balance
        const ok_bal = (await algodClient.accountInformation(ctc.address)).amount;
        prevRound = confirmedRound;
        return { didTimeout: false, data: args, value: value, balance: ok_bal, from: thisAcc.addr }; }

      debug(`${shad}: ${label} send ${funcNum} ${timeout_delay} --- FAIL/TIMEOUT`);
      const rec_res = await recv(label, timeout_evt, false, false, false, false, false);
      rec_res.didTimeout = true;
      return rec_res; };

    const recv = async (label, ok_num, timeout_delay, timeout_me, timeout_args, timeout_num, try_p) => {
      debug(`${shad}: ${label} recv ${ok_num} ${timeout_delay} --- START`);

      // XXX unclear if the first argument is the 
      algodClient.transactionByAddress(ctc.address, prevRound, prevRound + timeout_delay);

      // XXX have to figure out the actual round it was in
      prevRound = prevRound + timeout_delay;
      
      debug(`XXX recv`); };

    return { sendrecv, recv, address: thisAcc.addr }; };

  const deploy = async (bin) => {
    debug(`${shad}: deploy`);
    
    const { LogicSigProgramB64, ApprovalProgramB64, ClearStateProgramB64 } = bin.ALGO;

    debug(`${shad}: deploy: making account`);
    const ctc_acc = algosdk.generateAccount();
    // XXX create the logic sig account
    
    debug(`${shad}: deploy: filling transaction`);
    const preTxn = await fillTxn( default_range_width, {
      "from": thisAcc.addr,
      "type": "appl",
      "ApplicationId": 0,
      "OnCompletion": "noOp",
      "ApplicationArgs": algosdk.encodeObj([ 0, ctc_acc.addr ]),
      "ApprovalProgram": ApprovalProgram,
      "ClearStateProgram": ClearStateProgram,
      "LocalStateSchema": 0,
      "GlobalStateSchema": algosdk.encodeObj({ "NumByteSlice": 2 }),
      // FIXME: SDK should allow me to not include these
      "Accounts": 0,
      "ForeignApps": 0
    } );
    debug(`${shad}: deploy: signing transction`);
    const signedTxn = algosdk.signTransaction(preTxn, from.sk);
    debug(`${shad}: deploy: txn signed, sending`);
    const confirmedTxn = await sendAndConfirm(signedTxn);
    debug(`${shad}: deploy: application confirmed`);
    const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
    const creationRound = confirmTxn.round;

    const ctc = { address: ctc_acc.addr, appId, creationRound };

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
