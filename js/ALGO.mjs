import algosdk from 'algosdk';

const token = process.env.ALGO_TOKEN || "88f05de47936c12a756a28012aba3a24c3d06c45acd27fdcadb2a08ba32dc658";
const server = process.env.ALGO_SERVER || "http://localhost";
const port = process.env.ALGO_PORT || 8080;
const algodClient = new algosdk.Algod(token, server, port);

const valid_range_width = 1000;

const FAUCET = algosdk.mnemonicToSecretKey((process.env.ALGO_FAUCET_PASSPHRASE || "truck lobster turn fish foot paper select enough basket scout rack swallow chuckle laugh lava trumpet evidence pottery range news satoshi want popular absent math"));

// Helpers
const DEBUG = true;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG: ${msg}`); } };

const panic = e => { throw Error(e); };

const waitForConfirmation = async (txId) => {
  while (true) {
    const lastRound = (await algodClient.status()).lastRound;
    const pendingInfo = await algodClient.pendingTransactionInformation(txId);
    if (pendingInfo.round != null && pendingInfo.round > 0) {
      return pendingInfo; }
    await algodClient.statusAfterBlock(lastRound + 1); } };

const sendAndConfirm = async (txn) => {
  await algodClient.sendRawTransaction(txn.blob)
  return await waitForConfirmation(txn.txID); }

// Client API
export const assert = d => nodeAssert.strict(d);

// Backend
const fillTxn = async ( txn ) => {
  const params = await algodClient.getTransactionParams();;
  return Object.assign(txn,
                       { "fee": params.minFee,
                         "firstRound": params.lastRound,
                         "lastRound": params.lastRound + valid_range_width,
                         "genesisID": params.genesisID,
                         "genesisHash": params.genesishashb64 }); };

export const transfer = async (to, from, value) => {
  const txn = await fillTxn( {
    "type": "pay",
    "from": from.addr,
    "to": to.addr,
    "amount": value } );
  const signedTxn = algosdk.signTransaction(txn, from.sk);
  await sendAndConfirm( signedTxn ); };

export const connectAccount = async address => {
  const attach = async (bin, ctc) => {
    const { LogicSigProgram } = bin.ALGO;
    
    const sendrecv = async (label, funcName, args, value, ok_evt, timeout_delay, timeout_evt, try_p) => {
      // XXX ${gcmd} app call --app-id $APPID --app-arg "str:check" --app-arg "str:bar" --from $ACCOUNT

      const appTxn = await fillTxn( {
        "from": address,
        "type": "appl",
        "ApplicationId": ctc.appId,
        "OnCompletion": "noOp",
        "ApplicationArgs": "XXX",
        // FIXME: SDK should allow me to not include these
        "Accounts": algosdk.encodeObj([]),
        "ForeignApps": algosdk.encodeObj([]),
        "ApprovalProgram": 0,
        "ClearStateProgram": 0,
        "LocalStateSchema": 0,
        "GlobalStateSchema": 0
      } );

      debug(`XXX sendrecv`); };

    const recv = async (label, ok_evt, timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt, try_p) => {
      debug(`XXX recv`); };

    return { sendrecv, recv, address }; };

  const deploy = async (bin) => {
    const { LogicSigProgram, ApprovalProgram, ClearStateProgram } = bin.ALGO;

    const ctc_acc = algosdk.generateAccount();
    // XXX create the logic sig account
    
    const preTxn = await fillTxn( {
      "from": address,
      "type": "appl",
      "ApplicationId": 0,
      "OnCompletion": "noOp",
      // XXX Maybe make this sender? and remove argument?
      "ApplicationArgs": algosdk.encodeObj([ 0, ctc_acc.addr ]),
      "ApprovalProgram": ApprovalProgram,
      "ClearStateProgram": ClearStateProgram,
      "LocalStateSchema": algosdk.encodeObj({ NumUInt: 0, NumByteSlice: 0 }),
      "GlobalStateSchema": algosdk.encodeObj({ NumUInt: 0, NumByteSlice: 2 }),
      // FIXME: SDK should allow me to not include these
      "Accounts": algosdk.encodeObj([]),
      "ForeignApps": algosdk.encodeObj([])
    } );
    const signedTxn = algosdk.signTransaction(preTxn, from.sk);
    const confirmedTxn = await sendAndConfirm(signedTxn);
    const appId = confirmedTxn.TransactionResults.CreatedAppIndex;
    const creationRound = confirmTxn.TransactionResults.ConfirmedRound;

    const ctc = { address: ctc_acc.addr, appId, creationRound };

    return attach(bin, ctc); };

  return { deploy, attach, address }; };

const showBalance = async (note, acc) => {
  let theInfo = await algodClient.accountInformation(acc.addr);
  console.log("%s: balance: %d microAlgos", note, theInfo.amount) };

export const newTestAccount = async (startingBalance) => {
  const acc = algosdk.generateAccount();
  showBalance("before", acc);
  await transfer(acc, FAUCET, startingBalance);
  showBalance("after", acc);
  return connectAccount(acc); };
