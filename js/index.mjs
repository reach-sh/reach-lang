import Web3            from 'web3';
import * as crypto     from 'crypto';
import * as nodeAssert from 'assert';
import ethers          from 'ethers';

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';
const web3 = new Web3(new Web3.providers.HttpProvider(uri));

const panic = e => { throw Error(e); };

const k = (reject, f) => (err, ...d) =>
      !!err ? reject(err)
  : f(...d);

const flip = (f, a, b) =>
      f(b, a);

const un0x           = h => h.replace(/^0x/, '');
const hexTo0x        = h => '0x' + h.replace(/^0x/, '');
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');

const DEBUG = false;
const debug = msg => { if (DEBUG) {
  console.log(`DEBUG: ${msg}`); } };

const nat_to_fixed_size_hex = size => n => {
  const err = m => panic(`nat_to_fixed_size_hex: ${m}`);

  const notNat = !(Number.isInteger(n) && 0 <= n);
  const tooBig = !(Math.ceil(Math.log2(n + 1) / 8) <= size);

  return notNat ? err(`expected a nat`)
    : tooBig ? err(`expected a nat that fits into ${size} bytes`)
    : n.toString(16).padStart((2 * size), '0');
};

// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex =
      nat_to_fixed_size_hex(2);

export const balanceOf = a =>
  web3.eth.getBalance(a.userAddress)
  .then(toBN);

export const assert = d => nodeAssert.strict(d);

export const toWei     = web3.utils.toWei;
export const toBN      = web3.utils.toBN;
export const toWeiBN = (a,b) => toBN(toWei(a, b));
export const isBN      = web3.utils.isBN;
export const keccak256 = web3.utils.keccak256;

export const hexToBN          = h => toBN(hexTo0x(h));
export const uint256_to_bytes = i => bnToHex(i);

export const bnToHex = (u, size = 32) =>
  toBN(u)
  .toTwos(8 * size)
  .toString(16, 2 * size);

const hexOf = x =>
      typeof x === 'string' && x.slice(0, 2) === '0x'
      ? un0x(web3.utils.toHex(x))
      : un0x(web3.utils.toHex(`0x${x}`));

export const bytes_eq = (x, y) =>
  hexOf(x) === hexOf(y);

export const bytes_len = b => {
  const bh = hexOf(b);
  const n  = bh.length / 2;

  return !Number.isInteger(n)
    ? panic(`Invalid byte string: ${bh}`)
    : n;
};

// ∀ a b, msg_left (msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
export const bytes_cat = (a, b) => {
  const ah = hexOf(a);
  const bh = hexOf(b);
  const n  = nat16_to_fixed_size_hex(bytes_len(ah));

  return '0x' +  n + ah + bh;
};

export const random_uint256 = () =>
  hexToBN(byteArrayToHex(crypto.randomBytes(32)));

export const eq = (a, b) => toBN(a).eq( toBN(b));
export const equal = eq;
export const add   = (a, b) => toBN(a).add(toBN(b));
export const sub   = (a, b) => toBN(a).sub(toBN(b));
export const mod   = (a, b) => toBN(a).mod(toBN(b));
export const mul   = (a, b) => toBN(a).mul(toBN(b));
export const ge    = (a, b) => toBN(a).gte(toBN(b));
export const gt    = (a, b) => toBN(a).gt( toBN(b));
export const le    = (a, b) => toBN(a).lte(toBN(b));
export const lt    = (a, b) => toBN(a).lt( toBN(b));

const checkType = (t, x) => {
  if ( t === 'bool' ) { return typeof(x) === 'boolean'; }
  else if ( t === 'uint256' ) { return web3.utils.isBN(t); }
  else if ( t === 'bytes' ) { return web3.utils.isHex(t) || typeof(x) === 'string'; } };
export const isType = (t, x) => {
  if ( checkType(t, x) ) { return x; }
  else { panic(`Expected ${t}, got: "${x}"`); } };

// `t` is a type name in string form; `v` is the value to cast
export const encode = (t, v) =>
  ethers.utils.defaultAbiCoder.encode([t], [v]);

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

// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
export const transfer = (to, from, value) =>
  web3.eth.sendTransaction({ to, from, value });

// https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
const mkSendRecv =
      (ctc, address, from, ctors) => async (label, funcName, args, value, eventName, timeout_delay, timeout_evt ) => {
        void(eventName);
        // XXX
        void(timeout_delay, timeout_evt);
        // https://github.com/ethereum/web3.js/issues/2077
        const munged = [ ctc.last_block, ...ctors, ...args ]
              .map(m => isBN(m) ? m.toString() : m);

        debug(`send ${label} ${funcName}: start (${ctc.last_block})`);
        // XXX Will this retry until it works?
        return new web3.eth.Contract(ctc.abi, address)
          .methods[funcName](...munged)
          .send({ from, value })
          .on('error', (err, r) =>
              // XXX I think this is how a failed assertion shows up
              panic(`Error from contract: ${label} ${funcName}: ${err} ${r}`))
          .then(r => { debug(`send ${label} ${funcName}: check receipt`);
                       return fetchAndRejectInvalidReceiptFor(r.transactionHash); })
          .then(r => { const this_block = r.blockNumber;
                       ctc.last_block = this_block;
                       debug(`send ${label} ${funcName}: getBalance`);
                       return web3.eth.getBalance(address, this_block); } )
          .then(nbs => {
            debug(`send ${label} ${funcName}: stop`);
            return { didTimeout: false, value: value, balance: toBN(nbs) }; });
      };

const consumedEventKeyOf = (name, e) =>
      `${name}:${e.blockNumber}:${e.transactionHash}`;

// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = ctc => async (label, eventName, timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt ) => {
  let alreadyConsumed = false;
  // XXX
  void(timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt);

  const consume = (e, bns, resolve, reject) =>
        fetchAndRejectInvalidReceiptFor(e.transactionHash)
        .then(() => web3.eth.getTransaction(e.transactionHash, k(reject, t => {
          const key = consumedEventKeyOf(eventName, e);

          if (alreadyConsumed || (ctc.consumedEvents[key] !== undefined))
            return reject(`${label} has already consumed ${key}!`);

          // Sanity check: events ought to be consumed monotonically
          const latestPrevious = Object.values(ctc.consumedEvents)
                .filter(x => x.eventName === eventName)
                .sort((x, y) => x.blockNumber - y.blockNumber)
                .pop();

          if (!!latestPrevious && latestPrevious.blockNumber >= e.blockNumber) {
            reject(`${label} attempted to consume ${eventName} out of sequential block # order!`);
          }

          alreadyConsumed = true;
          Object.assign(ctc.consumedEvents, { [key]: Object.assign({}, e, { eventName }) });
          const this_block = t.blockNumber;
          ctc.last_block = this_block;
          return web3.eth.getBalance(ctc.address, this_block)
            .then(nbs => resolve({ didTimeout: false, data: bns, value: t.value, balance: toBN(nbs) }));
        })));

  const past = () =>
        new Promise((resolve, reject) =>
                    new web3.eth.Contract(ctc.abi, ctc.address)
                    .getPastEvents(eventName, { toBlock: 'latest' })
                    .then(es => {
                      const e = es
                            .find(x => ctc.consumedEvents[consumedEventKeyOf(eventName, x)] === undefined);

                      if (!e)
                        return reject();

                      const argsAbi = ctc.abi
                            .find(a => a.name === eventName)
                            .inputs;

                      const decoded = web3.eth.abi.decodeLog(argsAbi, e.raw.data, e.raw.topics);

                      const bns = argsAbi
                            .map(a => a.name)
                            .map(n => decoded[n]);

                      return consume(e, bns, resolve, reject);
                    }));

  const pollPast = () => new Promise(resolve => {
    const attempt = () => past()
          .then(resolve)
          .catch(() => flip(setTimeout, 500, () => !alreadyConsumed && attempt()));

    return attempt();
  });

  const next = () =>
        new Promise((resolve, reject) => new ethers
                    .Contract(ctc.address, ctc.abi, new ethers.providers.Web3Provider(web3.currentProvider))
                    .once(eventName, (...a) => {
                      const b = a.map(b => b); // Preserve `a` w/ copy
                      const e = b.pop();       // The final element represents an `ethers` event object

                      // Swap ethers' BigNumber wrapping for web3's
                      const bns = b.map(x => toBN(x.toString()));

                      return consume(e, bns, resolve, reject);
                    }));

  return past()
    .catch(() => Promise.race([ pollPast(), next() ]).catch(panic));
};

const attach = userAddress => (abi, ctors, address, creation_block) => {
  debug(`created at ${creation_block}`);
  const ctc =
        { abi:            abi
          , sendrecv:       undefined
          , recv:           undefined
          , consumedEvents: {}
          , creation_block: creation_block
          , last_block: creation_block
          , ctors
          , address
        };
  ctc.sendrecv = mkSendRecv(ctc, address, userAddress, ctors);
  ctc.recv = mkRecv(ctc);

  return ctc;
};

// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const mkDeploy = userAddress => (abi, bytecode, ctors) => {
  // TODO track down solid docs RE: why the ABI would have extra constructor
  // fields and when/how/why dropping leading `0x`s is necessary
  const ctorTypes = abi
        .find(a => a.type === 'constructor')
        .inputs
        .map(i => i.type)
        .slice(0, ctors.length);

  const encodedCtors = ctors
        .map(c => encode(ctorTypes[ctors.indexOf(c)], c))
        .map(un0x);

  const data = [ bytecode, ...encodedCtors ].join('');

  const contractFromReceipt = r =>
        attach(userAddress)(abi, ctors, r.contractAddress, r.blockNumber);

  return web3.eth.estimateGas({ data })
    .then(gas => web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(contractFromReceipt);
};

export const EthereumNetwork = userAddress =>
  ({ deploy: mkDeploy(userAddress)
     , attach: (abi, ctors, address, creation_block) => Promise.resolve(attach(userAddress)(abi, ctors, address, creation_block))
     , userAddress
   });

export const newTestAccount = async (startingBalance) => {
  const [ prefunder ] = await web3.eth.personal.getAccounts();

  const to = await web3.eth.personal.newAccount('');

  if ( await web3.eth.personal.unlockAccount(to, '', 999999999) ) {
    await transfer(to, prefunder, startingBalance);
    return EthereumNetwork(to); }
  else {
    throw Error(`Couldn't unlock account ${to}!`); } };
