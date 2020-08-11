import * as crypto     from 'crypto';
import * as nodeAssert from 'assert';
import ethers          from 'ethers';
import Timeout         from 'await-timeout';
import * as util       from 'util';
import * as waitPort   from 'wait-port';
import * as http       from 'http';
import * as url        from 'url';
void(util);

// networkAccount[ETH] = {
//   // Required for receivers
//   address: string
//
//   // Required for senders
//   sendTransaction: function
//
//   // Must be an ethers.Wallet to deploy or attach to a contract.
// }
//
// ctc[ETH] = {
//   address: string
//   creation_block: int
//
//   // internal fields
//   // * not required to call acc.attach(bin, ctc)
//   // * required by backend
//   sendrecv: function
//   recv: function
// }


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

export const T_Null = (v) => v == null;

export const T_Bool = (v) => typeof(v) === 'boolean';

export const T_UInt256 = (v) => isBN(v);

export const T_Bytes = (x) => typeof(x) === 'string';

export const T_Address = (x) => isHex(x) || typeof(x) === 'string';

export const T_Array = (ctcs) => (args) => {
  if (ctcs.length != args.length) { return false; }
  for ( let i = 0; i < ctcs.length; i++ ) {
    if ( ! ctcs[i](args[i]) ) { return false; } }
  return true; }

export const T_Object = (co) => (vo) => {
  for ( const prop in co ) {
    if ( ! co[prop](vo[prop]) ) { return false; } }
  return true; }

export const protect = (how, what) => {
  if ( how(what) ) { return what; }
  else { panic(`Expected ${how}, got: "${what}"`); } };

export const assert = d => nodeAssert.strict(d);

// XXX export the ethers names for these things?
const {
  BigNumber,
  bigNumberify,
  hexlify,
  toUtf8Bytes,
  toUtf8String,
  isHexString,
} = ethers.utils;
const { isBigNumber } = BigNumber;

export const toBN = bigNumberify;
export const isBN = isBigNumber;

// Massage the arg into a form keccak256 will handle correctly
const kek = (arg) => {
  if (typeof(arg) === 'string') {
    if (isHex(arg)) {
      return arg;
    } else {
      return toUtf8Bytes(arg);
    }
  } else if (typeof(arg) === 'number') {
    return '0x' + bnToHex(arg);
  } else if (isBN(arg)) {
    return '0x' + bnToHex(arg);
  } else if (arg && arg.constructor && arg.constructor.name == 'Uint8Array'){
    return arg;
  } else {
    return panic(`Can't kek this: ${arg}`);
  }
};

export const toHex = (x) => hexlify(kek(x));
export const isHex = isHexString;
export const hexToString = toUtf8String;

export const keccak256 = (...args) => {
  const kekArgs = args.map(kek);
  const kekArrs = kekArgs.map(ethers.utils.arrayify);
  const kekCat = ethers.utils.concat(kekArrs);
  return ethers.utils.keccak256(kekCat);
};

export const hexToBN = h => toBN(hexTo0x(h));
export const uint256_to_bytes = i => bnToHex(i);

// size is in bytes; default size 32 = 256 bytes
export const bnToHex = (u, size = 32) => {
  const nPos = bigNumberify(u).toTwos(8 * size);
  const nArr = ethers.utils.padZeros(nPos, size);
  // XXX why do we slice off the 0x?
  return hexlify(nArr).slice(2);
};

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

export const toWei = (amt, unit) =>
  ethers.utils.parseUnits(amt, unit || 'ether');
export const fromWei = (amt, unit) =>
  ethers.utils.formatUnits(amt, unit || 'ether');
export const toWeiBN = (a,b) => toBN(toWei(a, b));

// end Unique helpers

// private helpers

// XXX: clients might not want this
process.on('unhandledRejection', error => {
  console.log('Unhandled Rejection detected!!!!!');
  console.log(error);
  process.exit(1);
});

const flaky = async (f) => {
  const max_tries = 3;
  const sleep_between_tries = 1000; // ms
  let failed_attempts = 0;
  while (true) {
    try {
      // await doHealthcheck();
      return await f();
    } catch (e) {
      failed_attempts++;
      if (failed_attempts >= max_tries) {
        throw e;
      } else {
        debug(`FAILED ATTEMPT # ${failed_attempts}...`);
        await Timeout.set(sleep_between_tries);
        debug('trying again...');
      }
    }
  }
};
void(flaky); // XXX

// end private helpers

// Common interface exports

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
const network = process.env.ETH_NODE_NETWORK || 'unspecified';

const portP = (async () => {
  const { hostname, port, path } = url.parse(uri);
  const params = {
    protocol: 'http' // XXX no apparent need to support https
    , host: hostname
    , port: parseInt(port, 10)
    , path
    , 'output': 'silent'
    , 'timeout': 1000*60*1 };
  return await waitPort.default(params);
})();

// XXX: doesn't even retry, just returns the first attempt
const doHealthcheck = async () => {
  return new Promise((resolve, reject) => {
    const { hostname, port } = url.parse(uri);
    const data = JSON.stringify({
      jsonrpc: '2.0',
      method: 'web3_clientVersion',
      params: [],
      id: 67
    });
    debug('Sending health check request...');
    const opts = {
      hostname,
      port,
      path: '/',
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': data.length,
      }
    };
    const req = http.request(opts, (res) => {
      debug(`statusCode: ${res.statusCode}`);
      res.on('data', (d) => {
        debug('rpc health check succeeded');
        if (DEBUG) {
          process.stdout.write(d);
        }
        resolve({res, d});
      });
    });
    req.on('error', (e) => {
      console.log('rpc health check failed');
      console.log(e);
      reject(e);
    });
    req.write(data);
    debug('attached all the handlers...');
    req.end();
    debug('req.end...');
  });
};

const devnetP = (async () => {
  await portP;
  debug('Got portP, waiting for health');
  return await doHealthcheck();
})();

const etherspP = (async () => {
  await devnetP;
  // Note: `network` must not be undefined. 'unspecified' is ok.
  // This is specific to ethers v4. Supposedly fixed in ethers v5.
  // https://github.com/ethers-io/ethers.js/issues/274
  const ethersp = new ethers.providers.JsonRpcProvider(uri, network);
  ethersp.pollingInterval = 500; // ms
  return ethersp;
})();

// XXX expose setProvider

const ethersBlockOnceP = async () => {
  const ethersp = await etherspP;
  return new Promise((resolve) => ethersp.once('block', (n) => resolve(n)));
};

export const balanceOf = async acc => {
  const { networkAccount } = acc;
  if (!networkAccount) panic(`acc.networkAccount missing. Got: ${acc}`);

  if (networkAccount.getBalance) {
    return toBN(await acc.networkAccount.getBalance());
  } else if (networkAccount.address) {
    const ethersp = await etherspP;
    return toBN(await ethersp.getBalance(networkAccount.address));
  } else return panic(`acc.networkAccount.address missing. Got: ${networkAccount}`);
};

// XXX dead code?
// `t` is a type name in string form; `v` is the value to cast
// const encode = (t, v) =>
//   ethers.utils.defaultAbiCoder.encode([t], [v]);

export const transfer = async (to, from, value) => {
  if (!to.address) panic(`Expected to.address: ${to}`);
  if (!from.sendTransaction) panic(`Expected from.sendTransaction: ${from}`);
  if (!isBN(value)) panic(`Expected a BN: ${value}`);

  const txn = { to: to.address, value: toHex(value) };
  debug(`from.sendTransaction(${JSON.stringify(txn)})`);
  return await from.sendTransaction(txn);
};

// Helpers for sendrecv and recv

const rejectInvalidReceiptFor = async (txHash, r) =>
      new Promise((resolve, reject) =>
                  !r                             ? reject(`No receipt for txHash: ${txHash}`)
                  : r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
                  : !r.status                    ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`)
                  : resolve(r));

const fetchAndRejectInvalidReceiptFor = async txHash => {
  const ethersp = await etherspP;
  const r = await ethersp.getTransactionReceipt(txHash);
  return await rejectInvalidReceiptFor(txHash, r);
};

export const connectAccount = async networkAccount => {
  // XXX networkAccount MUST be a wallet to deploy/attach
  const ethersp = await etherspP;
  const { address } = networkAccount;
  const shad = address.substring(2,6);

  const attach = async (bin, ctc) => {
    const ctc_address = ctc.address;
    const creation_block = ctc.creation_block;
    const ABI = JSON.parse(bin.ETH.ABI);
    const ethersCtc = new ethers.Contract(ctc_address, ABI, networkAccount);
    const eventOnceP = (e) =>
          new Promise((resolve) => ethersCtc.once(e, (...a) => resolve(a)));

    debug(`${shad}: created at ${creation_block}`);
    let last_block = creation_block;

    const updateLast = o => { last_block = o.blockNumber; };

    // XXX Dumb hack because ethers sometimes uses BigNumber not BN
    // Anything that's not a BigNumber will come back out untouched.
    const bn2bn = (n) => {
      if (n && n.constructor && n.constructor.name == 'BigNumber') {
        return toBN(n);
      } else return n;
    };

    const getEventData = (ok_evt, ok_e) => {
      const ok_args_abi = ethersCtc.interface.events[ok_evt].inputs;
      const { values } = ethersCtc.interface.parseLog(ok_e);
      const [ ok_bal, ...ok_vals ] = ok_args_abi.map(a => bn2bn(values[a.name]));

      return [ ok_bal, ok_vals ];
    };

    const sendrecv_top = async (label, funcNum, evt_cnt, args, value, timeout_delay, try_p) => {
      void(try_p, evt_cnt);
      return sendrecv(label, funcNum, args, value, timeout_delay); };

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
        try {
          const r_fn = await ethersCtc[funcName](...munged, {value: toHex(value)});
          r_maybe = await r_fn.wait();
        } catch (e) {
          debug(e);
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
        const es = await ethersp.getLogs({
          fromBlock: block_poll_start,
          toBlock: block_poll_end,
          address: ethersCtc.address,
          topics: [ethersCtc.interface.events[ok_evt].topic],
        });
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
          const ok_t = await ethersp.getTransaction(ok_e.transactionHash);
          debug(`${ok_evt} gas was ${ok_t.gas} ${ok_t.gasPrice}`);

          updateLast(ok_t);
          const [ ok_bal, ok_vals ] = getEventData(ok_evt, ok_e);
          return { didTimeout: false, data: ok_vals, value: ok_t.value, balance: ok_bal, from: ok_t.from }; } }

      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- TIMEOUT`);
      const rec_res = {};
      rec_res.didTimeout = true;
      return rec_res; };

    return { ...ctc, sendrecv: sendrecv_top, recv: recv_top }; };

  // Not sure where the v4 contract docs are but this was just as good
  // https://docs.ethers.io/ethers.js/v5-beta/api-contract.html#deployment
  const deploy = async (bin) => {
    const { ABI, Bytecode } = bin.ETH;
    const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);
    const contract = await factory.deploy();
    await contract.deployed(); // Wait for it to actually be deployed.
    const deployTxn = await networkAccount.provider.getTransaction(contract.deployTransaction.hash);
    // XXX the equivalent of rejectInvalidReceiptFor?
    // This may be handled already in contract.deployed()
    return await attach(bin, { address: contract.address, creation_block: deployTxn.blockNumber });
  };

  return { deploy, attach, networkAccount }; };

export const newTestAccount = async (startingBalance) => {
  debug(`newTestAccount(${startingBalance})`);
  const ethersp = await etherspP;
  const prefunder = ethersp.getSigner();

  const networkAccount = ethers.Wallet.createRandom().connect(ethersp);
  const to = networkAccount.address;

  try {
    debug(`awaiting transfer: ${to}`);
    await transfer(networkAccount, prefunder, startingBalance);
    debug(`got transfer. awaiting connectAccount: ${to}`);
    const acc = await connectAccount(networkAccount);
    debug(`got connectAccount: ${to}`);
    return acc;
  } catch (e) {
    console.log(`Trouble with account ${to}`);
    throw e;
  }
};
