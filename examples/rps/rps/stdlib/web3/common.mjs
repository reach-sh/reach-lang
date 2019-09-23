// vim: filetype=javascript

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


// Parameterized ///////////////////////////////////////////////////////////////

const balanceOf = A => a =>
      A.web3.eth.getBalance(a.userAddress)
      .then(toBN(A));

const assert = ({ asserter }) => d => asserter(d);

const toWei     = ({ web3 }) => web3.utils.toWei;
const toBN      = ({ web3 }) => web3.utils.toBN;
const isBN      = ({ web3 }) => web3.utils.isBN;
const keccak256 = ({ web3 }) => web3.utils.keccak256;

const hexToBN          = A => h => toBN(A)(hexTo0x(h));
const uint256_to_bytes = A => i => bnToHex(A)(i);


const bnToHex = A => (u, size = 32) =>
      toBN(A)(u)
      .toTwos(8 * size)
      .toString(16, 2 * size);


const hexOf = ({ web3 }) => x =>
      typeof x === 'string' && x.slice(0, 2) === '0x'
      ? un0x(web3.utils.toHex(x))
      : un0x(web3.utils.toHex(`0x${x}`));


const bytes_eq = A => (x, y) =>
      hexOf(A)(x) === hexOf(A)(y);


const bytes_len = A => b => {
  const bh = hexOf(A)(b);
  const n  = bh.length / 2;

  return !Number.isInteger(n)
    ? panic(`Invalid byte string: ${bh}`)
    : n;
};


// ∀ a b, msg_left (msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
const bytes_cat = A => (a, b) => {
  const ah = hexOf(A)(a);
  const bh = hexOf(A)(b);
  const n  = nat16_to_fixed_size_hex(bytes_len(A)(ah));

  return '0x' +  n + ah + bh;
};


const random_uint256 = A => () =>
      hexToBN(A)(byteArrayToHex(A.random32Bytes()));


const equal = A => (a, b) => toBN(A)(a).eq( toBN(A)(b));
const add   = A => (a, b) => toBN(A)(a).add(toBN(A)(b));
const sub   = A => (a, b) => toBN(A)(a).sub(toBN(A)(b));
const mod   = A => (a, b) => toBN(A)(a).mod(toBN(A)(b));
const mul   = A => (a, b) => toBN(A)(a).mul(toBN(A)(b));
const ge    = A => (a, b) => toBN(A)(a).gte(toBN(A)(b));
const gt    = A => (a, b) => toBN(A)(a).gt( toBN(A)(b));
const le    = A => (a, b) => toBN(A)(a).lte(toBN(A)(b));
const lt    = A => (a, b) => toBN(A)(a).lt( toBN(A)(b));

const checkType = ({ web3 }) => (t, x) => {
  if ( t === 'bool' ) { return typeof(x) === 'boolean'; }
  else if ( t === 'uint256' ) { return web3.utils.isBN(t); }
  else if ( t === 'bytes' ) { return web3.utils.isHex(t) || typeof(x) === 'string'; } };
const isType = A => (t, x) => {
  if ( checkType(A)(t, x) ) { return x; }
  else { panic(`Expected ${t}, got: "${x}"`); } };

// `t` is a type name in string form; `v` is the value to cast
const encode = ({ ethers }) => (t, v) =>
      ethers.utils.defaultAbiCoder.encode([t], [v]);


const rejectInvalidReceiptFor = txHash => r => new Promise((resolve, reject) =>
                                                           !r                           ? reject(`No receipt for txHash: ${txHash}`)
                                                           : r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
                                                           : !r.status                    ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`)
                                                           : resolve(r));


const fetchAndRejectInvalidReceiptFor = ({ web3 }) => txHash =>
      web3.eth.getTransactionReceipt(txHash)
      .then(rejectInvalidReceiptFor(txHash));


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const transfer = ({ web3 }) => (to, from, value) =>
      web3.eth.sendTransaction({ to, from, value });


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
const mkSendRecv =
      A => (ctc, address, from, ctors) => async (label, funcName, args, value, eventName, timeout_delay, timeout_evt ) => {
        void(eventName);
        // XXX
        void(timeout_delay, timeout_evt);
        // https://github.com/ethereum/web3.js/issues/2077
        const munged = [ ctc.last_block, ...ctors, ...args ]
              .map(m => isBN(A)(m) ? m.toString() : m);

        debug(`send ${label} ${funcName}: start (${ctc.last_block})`);
        // XXX Will this retry until it works?
        return new A.web3.eth.Contract(A.abi, address)
          .methods[funcName](...munged)
          .send({ from, value })
          .on('error', (err, r) =>
              // XXX I think this is how a failed assertion shows up
              panic(`Error from contract: ${label} ${funcName}: ${err} ${r}`))
          .then(r => { debug(`send ${label} ${funcName}: check receipt`);
                       return fetchAndRejectInvalidReceiptFor(A)(r.transactionHash); })
          .then(r => { const this_block = r.blockNumber;
                       ctc.last_block = this_block;
                       debug(`send ${label} ${funcName}: getBalance`);
                       return A.web3.eth.getBalance(address, this_block); } )
          .then(nbs => {
            debug(`send ${label} ${funcName}: stop`);
            return { didTimeout: false, value: value, balance: toBN(A)(nbs) }; });
      };


const consumedEventKeyOf = (name, e) =>
      `${name}:${e.blockNumber}:${e.transactionHash}`;


// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = ({ web3, ethers }) => c => async (label, eventName, timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt ) => {
  let alreadyConsumed = false;
  // XXX
  void(timeout_delay, timeout_me, timeout_args, timeout_fun, timeout_evt);

  const consume = (e, bns, resolve, reject) =>
        fetchAndRejectInvalidReceiptFor({ web3 })(e.transactionHash)
        .then(() => web3.eth.getTransaction(e.transactionHash, k(reject, t => {
          const key = consumedEventKeyOf(eventName, e);

          if (alreadyConsumed || (c.consumedEvents[key] !== undefined))
            return reject(`${label} has already consumed ${key}!`);

          // Sanity check: events ought to be consumed monotonically
          const latestPrevious = Object.values(c.consumedEvents)
                .filter(x => x.eventName === eventName)
                .sort((x, y) => x.blockNumber - y.blockNumber)
                .pop();

          if (!!latestPrevious && latestPrevious.blockNumber >= e.blockNumber) {
            reject(`${label} attempted to consume ${eventName} out of sequential block # order!`);
          }

          alreadyConsumed = true;
          Object.assign(c.consumedEvents, { [key]: Object.assign({}, e, { eventName }) });
          const this_block = t.blockNumber;
          c.last_block = this_block;
          return web3.eth.getBalance(c.address, this_block)
            .then(nbs => resolve({ didTimeout: false, data: bns, value: t.value, balance: toBN({ web3 })(nbs) }));
        })));

  const past = () =>
        new Promise((resolve, reject) =>
                    new web3.eth.Contract(c.abi, c.address)
                    .getPastEvents(eventName, { toBlock: 'latest' })
                    .then(es => {
                      const e = es
                            .find(x => c.consumedEvents[consumedEventKeyOf(eventName, x)] === undefined);

                      if (!e)
                        return reject();

                      const argsAbi = c.abi
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
                    .Contract(c.address, c.abi, new ethers.providers.Web3Provider(web3.currentProvider))
                    .once(eventName, (...a) => {
                      const b = a.map(b => b); // Preserve `a` w/ copy
                      const e = b.pop();       // The final element represents an `ethers` event object

                      // Swap ethers' BigNumber wrapping for web3's
                      const bns = b.map(x => toBN({ web3 })(x.toString()));

                      return consume(e, bns, resolve, reject);
                    }));

  return past()
    .catch(() => Promise.race([ pollPast(), next() ]).catch(panic));
};


const Contract = A => userAddress => (ctors, address, creation_block) => {
  debug(`created at ${creation_block}`);
  const c =
        { abi:            A.abi
          , bytecode:       A.bytecode
          , sendrecv:       undefined
          , recv:           undefined
          , consumedEvents: {}
          , creation_block: creation_block
          , last_block: creation_block
          , ctors
          , address
        };
  c.sendrecv = mkSendRecv(A)(c, address, userAddress, ctors);
  c.recv = mkRecv(A)(c);

  return c;
};


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const mkDeploy = A => userAddress => ctors => {
  // TODO track down solid docs RE: why the ABI would have extra constructor
  // fields and when/how/why dropping leading `0x`s is necessary
  const ctorTypes = A.abi
        .find(a => a.type === 'constructor')
        .inputs
        .map(i => i.type)
        .slice(0, ctors.length);

  const encodedCtors = ctors
        .map(c => encode(A.ethers)(ctorTypes[ctors.indexOf(c)], c))
        .map(un0x);

  const data = [ A.bytecode, ...encodedCtors ].join('');

  const contractFromReceipt = r =>
        Contract(A)(userAddress)(ctors, r.contractAddress, r.blockNumber);

  return A.web3.eth.estimateGas({ data })
    .then(gas => A.web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(contractFromReceipt);
};


const EthereumNetwork = A => userAddress =>
      ({ deploy: mkDeploy(A)(userAddress)
         , attach: (ctors, address, creation_block) => Promise.resolve(Contract(A)(userAddress)(ctors, address, creation_block))
         , web3:   A.web3
         , userAddress
       });


// devnet-specific /////////////////////////////////////////////////////////////

// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account
// function (which is also what its prefunder script uses)
const prefundedDevnetAcct = ({ web3 }) => () =>
      web3.eth.personal.getAccounts()
      .then(a => a[0])
      .catch(e => panic(`Cannot infer prefunded account!\n${e}`));


const createAndUnlockAcct = ({ web3 }) => () =>
      web3.eth.personal.newAccount('')
      .then(i => web3.eth.personal.unlockAccount(i, '', 999999999)
            .then(u => u ? Promise.resolve(i)
                  : Promise.reject(`Couldn't unlock account ${i}!`)));


////////////////////////////////////////////////////////////////////////////////


export const mkStdlib = A =>
  ({ hexTo0x
     , un0x
     , k
     , flip
     , web3:             A.web3
     , ethers:           A.ethers
     , balanceOf:        balanceOf(A)
     , random_uint256:   random_uint256(A)
     , uint256_to_bytes: uint256_to_bytes(A)
     , bytes_cat:        bytes_cat(A)
     , bytes_len:        bytes_len(A)
     , bytes_eq:         bytes_eq(A)
     , keccak256:        keccak256(A)
     , isType:           isType(A)
     , assert:           assert(A)
     , equal:            equal(A)
     , eq:               equal(A)
     , add:              add(A)
     , sub:              sub(A)
     , mod:              mod(A)
     , mul:              mul(A)
     , ge:               ge(A)
     , gt:               gt(A)
     , le:               le(A)
     , lt:               lt(A)
     , encode:           encode(A)
     , toWei:            toWei(A)
     , toBN:             toBN(A)
     , bnToHex:          bnToHex(A)
     , isBN:             isBN(A)
     , transfer:         transfer(A)
     , Contract:         Contract(A)
     , EthereumNetwork:  EthereumNetwork(A)

     , devnet: { prefundedDevnetAcct: prefundedDevnetAcct(A)
                 , createAndUnlockAcct: createAndUnlockAcct(A)
               }
   });
