import Web3            from 'web3';
import * as crypto     from 'crypto';
import * as nodeAssert from 'assert';
import ethers          from 'ethers';
import Timeout         from 'await-timeout';
import * as util       from 'util';
import * as waitPort   from 'wait-port';
void(util);


// Shared/copied code begins

const DEBUG = false;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG: ${msg}`); } };

const panic = e => { throw Error(e); };

const un0x           = h => h.replace(/^0x/, '');
const hexTo0x        = h => '0x' + h.replace(/^0x/, '');
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');

const hexOf = x =>
      typeof x === 'string' && x.slice(0, 2) === '0x'
      ? un0x(toHex(x))
      : un0x(toHex(`0x${x}`));

const checkType = (t, x) => {
  if ( t === 'bool' ) { return typeof(x) === 'boolean'; }
  else if ( t === 'uint256' ) { return isBN(x); }
  else if ( t === 'bytes' || t === 'address' ) { return isHex(x) || typeof(x) === 'string'; }
  else { panic(`Unknown type: ${t}`); } };

export const isType = (t, x) => {
  if ( checkType(t, x) ) { return x; }
  else { panic(`Expected ${t}, got: "${x}"`); } };

export const assert = d => nodeAssert.strict(d);

export const toBN = Web3.utils.toBN;
export const isBN = Web3.utils.isBN;
export const toHex = Web3.utils.toHex;
export const isHex = Web3.utils.isHex;
export const keccak256 = Web3.utils.soliditySha3;

export const hexToBN = h => toBN(hexTo0x(h));
export const uint256_to_bytes = i => bnToHex(i);

export const bnToHex = (u, size = 32) =>
  toBN(u)
  .toTwos(8 * size)
  .toString(16, 2 * size);

export const bytes_eq = (x, y) =>
  hexOf(x) === hexOf(y);

export const random_uint256 = () =>
  hexToBN(byteArrayToHex(crypto.randomBytes(32)));

export const eq    = (a, b) => toBN(a).eq( toBN(b));
export const add   = (a, b) => toBN(a).add(toBN(b));
export const sub   = (a, b) => toBN(a).sub(toBN(b));
export const mod   = (a, b) => toBN(a).mod(toBN(b));
export const mul   = (a, b) => toBN(a).mul(toBN(b));
export const div   = (a, b) => toBN(a).div(toBN(b));
export const ge    = (a, b) => toBN(a).gte(toBN(b));
export const gt    = (a, b) => toBN(a).gt( toBN(b));
export const le    = (a, b) => toBN(a).lte(toBN(b));
export const lt    = (a, b) => toBN(a).lt( toBN(b));

// end Shared/copied code


// Unique helpers

export const toWei = Web3.utils.toWei;
export const fromWei = Web3.utils.fromWei;
export const toWeiBN = (a,b) => toBN(toWei(a, b));

// end Unique helpers


// Common interface exports

/* BEGIN Hack */
// import * as extractTarget from 'wait-port/lib/extract-target';
function extractTarget(target) {
  if (!target) throw new Error('\'target\' is required');

  //  First, check to see if we have a protocol specified.
  const protocol = target.toLowerCase().startsWith('http://') ? 'http' : undefined;

  //  If we have a protocol, we can rip it out of the string.
  target = protocol ? target.substring('http://'.length) : target;

  //  If we have a protocol, we can also rip out the path (if there is one).
  const pathStart = target.indexOf('/');
  const path = pathStart !== -1 ? target.substring(pathStart) : undefined;
  target = pathStart !== -1 ? target.substring(0, pathStart) : target;

  //  Split the target by the separator (which might not be present.
  const split = target.split(':');
  if (split.length > 2) throw new Error(`'${target}' is an invalid target, it has more than two ':' symbols`);

  //  Grab the host and port (which will still be a string).
  const host = split.length === 2 ? (split[0] || undefined) : undefined;
  const portString = split.length === 1 ? split[0] : split[1];

  //  Make sure the port is numeric.
  if (!/^[0-9]+$/.test(portString)) throw new Error(`'${target}' is an invalid target, '${portString}' is not a valid port number - try something like 'host:port'`);
  const port = parseInt(portString, 10);

  //  That's it, return the extracted target.
  return { protocol, host, port, path };
}
/* END Hack */

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
(async () => {
  const { protocol, host, port, path } = extractTarget(uri);
  const params = {
    protocol, host, port, path
    , 'output': 'silent'
    , 'timeout': 1000*60*1 };
  await waitPort.default(params); })();

// XXX expose setProvider
const web3 = new Web3(new Web3.providers.HttpProvider(uri));
const ethersp = new ethers.providers.Web3Provider(web3.currentProvider);
ethersp.pollingInterval = 500; // ms
const ethersBlockOnceP = () =>
      new Promise((resolve) => ethersp.once('block', (n) => resolve(n)));

export const balanceOf = async a =>
  toBN(await web3.eth.getBalance(a.address));

// XXX dead code?
// `t` is a type name in string form; `v` is the value to cast
// const encode = (t, v) =>
//   ethers.utils.defaultAbiCoder.encode([t], [v]);

// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
export const transfer = async (to, from, value) =>
  await web3.eth.sendTransaction({ to, from, value });

// Helpers for sendrecv and recv

const rejectInvalidReceiptFor =
      txHash =>
      r =>
      new Promise((resolve, reject) =>
                  !r                           ? reject(`No receipt for txHash: ${txHash}`)
                  : r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
                  : !r.status                    ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`)
                  : resolve(r));

const fetchAndRejectInvalidReceiptFor = txHash =>
      web3.eth.getTransactionReceipt(txHash)
      .then(rejectInvalidReceiptFor(txHash));

export const connectAccount = address => {
  const shad = address.substring(2,6);

  const attach = (bin, ctc) => {
    const ctc_address = ctc.address;
    const creation_block = ctc.creation_block;
    const ABI = JSON.parse(bin.ETH.ABI);
    const ethCtc = new web3.eth.Contract(ABI, ctc_address);
    const ethersCtc = new ethers.Contract(ctc_address, ABI, ethersp);
    const eventOnceP = (e) =>
          new Promise((resolve) => ethersCtc.once(e, (...a) => resolve(a)));

    debug(`${shad}: created at ${creation_block}`);
    let last_block = creation_block;

    const updateLastAndGetEventData = async (o, ok_evt, ok_e) => {
      const this_block = o.blockNumber;
      last_block = this_block;

      const ok_args_abi = ABI
            .find(a => a.name === ok_evt)
            .inputs;
      const decoded = web3.eth.abi.decodeLog(ok_args_abi, ok_e.raw.data, ok_e.raw.topics);
      const [ ok_bal, ...ok_vals ] = ok_args_abi.map(a => a.name).map(n => decoded[n]);

      return [ ok_bal, ok_vals ]; };

    const sendrecv_top = async (label, funcNum, evt_cnt, args, value, timeout_delay, try_p) => {
      void(try_p, evt_cnt);
      return sendrecv(label, funcNum, args, value, timeout_delay); };

    // https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
    /* eslint require-atomic-updates: off */
    const sendrecv = async (label, funcNum, args, value, timeout_delay) => {
      const funcName = `m${funcNum}`;
      // https://github.com/ethereum/web3.js/issues/2077
      const munged = [ last_block, ...args ]
            .map(m => isBN(m) ? m.toString() : m);

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- START --- ${munged}`);
      let block_send_attempt = last_block;
      let block_repeat_count = 0;
      while ( ! timeout_delay || block_send_attempt < last_block + timeout_delay ) {
        let r_maybe = false;

        debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY`);
        try { r_maybe = await ethCtc.methods[funcName](...munged).send({ from: address, value }); }
        catch (e) {
          // XXX What should we do...? If we fail, but there's no timeout delay... then we should just die
          await Timeout.set(1);
          const current_block = await ethersp.getBlockNumber();
          if ( current_block == block_send_attempt ) {
            block_repeat_count++; }
          block_send_attempt = current_block;
          if ( timeout_delay && block_repeat_count > 32 ) {
            panic(`${shad}: ${label} send ${funcName} ${timeout_delay} --- REPEAT @ ${block_send_attempt} x ${block_repeat_count}`); }
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY FAIL --- ${last_block} ${current_block} ${block_repeat_count} ${block_send_attempt}`);
          continue; }

        assert(r_maybe != false);
        const ok_r = await fetchAndRejectInvalidReceiptFor(r_maybe.transactionHash);

        debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- OKAY`);

        // XXX It might be a little dangerous to rely on the polling to just work

        // It may be the case that the next line could speed things up?
        // last_block = ok_r.blockNumber;
        void(ok_r);

        return await recv( label, funcNum, timeout_delay ); }

      // XXX If we were trying to join, but we got sniped, then we'll
      // think that there is a timeout and then we'll wait forever for
      // the timeout message.

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- FAIL/TIMEOUT`);
      const rec_res = {};
      rec_res.didTimeout = true;
      return rec_res; };

    const recv_top = async (label, okNum, ok_cnt, timeout_delay) => {
      return recv(label, okNum, timeout_delay);
    };

    // https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
    const recv = async (label, okNum, timeout_delay) => {
      const ok_evt = `e${okNum}`;
      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- START`);

      let block_poll_start = last_block;
      let block_poll_end = block_poll_start;
      while ( ! timeout_delay || block_poll_start < last_block + timeout_delay ) {
        void(eventOnceP); // This might be nice for performance, but it may miss things too.
        const es = await ethCtc.getPastEvents(ok_evt, { fromBlock: block_poll_start, toBlock: block_poll_end });
        if ( es.length == 0 ) {
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- RETRY`);
          block_poll_start = block_poll_end;

          await Timeout.set(1);
          void(ethersBlockOnceP); // This might be a better option too, because we won't need to delay
          block_poll_end = await ethersp.getBlockNumber();

          continue;
        } else {
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY`);

          const ok_e = es[0];

          const ok_r = await fetchAndRejectInvalidReceiptFor(ok_e.transactionHash);
          void(ok_r);
          const ok_t = await web3.eth.getTransaction(ok_e.transactionHash);
          debug(`${ok_evt} gas was ${ok_t.gas} ${ok_t.gasPrice}`);

          const [ ok_bal, ok_vals ] = await updateLastAndGetEventData(ok_t, ok_evt, ok_e);
          return { didTimeout: false, data: ok_vals, value: ok_t.value, balance: ok_bal, from: ok_t.from }; } }

      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- TIMEOUT`);
      const rec_res = {};
      rec_res.didTimeout = true;
      return rec_res; };

    return { sendrecv: sendrecv_top, recv: recv_top, creation_block, address: ctc_address }; };

  // https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
  const deploy = async (bin) => {
    const data = bin.ETH.Bytecode;
    const gas = await web3.eth.estimateGas({ data });
    // FIXME have some way to have a link to the reach code
    const r = await web3.eth.sendTransaction({ data, gas, from: address });
    const r_ok = await rejectInvalidReceiptFor(r.transactionHash)(r);
    return attach(bin, { address: r_ok.contractAddress, creation_block: r_ok.blockNumber }); };

  return { deploy, attach, address }; };

export const newTestAccount = async (startingBalance) => {
  const [ prefunder ] = await web3.eth.personal.getAccounts();

  const to = await web3.eth.personal.newAccount('');

  if ( await web3.eth.personal.unlockAccount(to, '', 999999999) ) {
    await transfer(to, prefunder, startingBalance);
    return connectAccount(to); }
  else {
    panic(`Couldn't unlock account ${to}!`); } };
