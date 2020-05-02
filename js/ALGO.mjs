import algosdk from 'algosdk';

const token = process.env.ALGO_TOKEN || "88f05de47936c12a756a28012aba3a24c3d06c45acd27fdcadb2a08ba32dc658";
const server = process.env.ALGO_SERVER || "http://localhost";
const port = process.env.ALGO_PORT || 8080;
const algodClient = new algosdk.Algod(token, server, port);

const ROOT = algosdk.mnemonicToSecretKey((process.env.ALGO_ROOT_PASSPHRASE || "truck lobster turn fish foot paper select enough basket scout rack swallow chuckle laugh lava trumpet evidence pottery range news satoshi want popular absent math"));

// Helpers
const DEBUG = true;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG: ${msg}`); } };

const panic = e => { throw Error(e); };

const waitForConfirmation = async (txId) => {
  while (true) {
    let lastround = (await algodClient.status()).lastRound;
    let pendingInfo = await algodClient.pendingTransactionInformation(txId);
    if (pendingInfo.round != null && pendingInfo.round > 0) {
      break; }
    await algodClient.statusAfterBlock(lastround + 1); } };

// Client API
export const assert = d => nodeAssert.strict(d);

// Backend
export const transfer = async (to, from, value) => {
  let params = await algodClient.getTransactionParams();
  let txn = {
    "from": from.addr,
    "to": to.addr,
    "fee": params.minFee,
    "amount": value,
    "firstRound": params.lastRound,
    "lastRound": params.lastRound + 1000,
    "note": algosdk.encodeObj("Reach transfer"),
    "genesisID": params.genesisID,
    "genesisHash": params.genesishashb64
  };
  let signedTxn = algosdk.signTransaction(txn, from.sk);
  let txId = signedTxn.txID;
  await algodClient.sendRawTransaction(signedTxn.blob)

  await waitForConfirmation(txId); };

export const connectAccount = async address => {
  debug(`XXX connectAccount`);

  const attach = async (bin, ctc_address, creation_block) => {
    debug(`XXX attach`);

    const sendrecv = async (label, funcName, args, value, ok_evt, timeout_delay, timeout_evt, try_p) => {
      debug(`XXX sendrecv`); };

    const recv = async (label, ok_evt, timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt, try_p) => {
      debug(`XXX recv`); };

    return { sendrecv, recv, address }; };

  const deploy = async (bin) => {
    debug(`XXX deploy`); };

  return { deploy, attach, address }; };

export const newTestAccount = async (startingBalance) => {
  let acc = algosdk.generateAccount();

  { debug(`Before: account info`);
    let theInfo = await algodClient.accountInformation(acc.addr);
    console.log("Before balance: %d microAlgos", theInfo.amount) };
  await transfer(acc, ROOT, startingBalance);
  { debug(`After: account info`);
    let theInfo = await algodClient.accountInformation(acc.addr);
    console.log("After balance: %d microAlgos", theInfo.amount) };

  return connectAccount(acc); };
