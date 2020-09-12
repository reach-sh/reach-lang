import Timeout from 'await-timeout';

import * as stdlib from './shared.mjs';
export * from './shared.mjs';

const DEBUG = false;
const debug = msg => {
  if (DEBUG) {
    console.log(`DEBUG@${BLOCKS.length}: ${msg}`);
  }
};

const REACHY_RICH = { address: 'reachy_rich' };

// This can be exposed to the user for checking the trace of blocks
// for testing.
const BLOCKS = [];
const BALANCES = {};

export const balanceOf = async acc => {
  if (acc.networkAccount) { acc = acc.networkAccount; }
  return BALANCES[acc.address];
};

export const transfer = async (from, to, value) => {
  if (from.networkAccount) return await transfer(from.networkAccount, to, value);
  if (to.networkAccount) return await transfer(from, to.networkAccount, value);
  const toa = to.address;
  const froma = from.address;
  stdlib.assert(stdlib.le(value, BALANCES[froma]));
  debug(`transfer ${froma} -> ${toa} of ${value}`);
  BLOCKS.push({ to: toa, from: froma, value });
  BALANCES[toa] = stdlib.add(BALANCES[toa], value);
  BALANCES[froma] = stdlib.sub(BALANCES[froma], value);
  return null;
};

export const connectAccount = async networkAccount => {
  const { address } = networkAccount;

  const attach = async (bin, ctc) => {
    let last_block = ctc.creation_block;

    const iam = (some_addr) => {
      if (some_addr == address) {
        return address;
      } else {
        throw Error(`I should be ${some_addr}, but am ${address}`);
      }
    };

    const wait = (delta) => {
      // Don't wait from current time, wait from last_block
      waitUntilTime(stdlib.add(last_block, delta));
    };

    const sendrecv = async (label, funcNum, evt_cnt, tys, args, value, timeout_delay, try_p) => {
      // XXX use try_p to figure out what transfers from the contract
      // to make, like in ALGO
      void(tys, try_p);

      if (!timeout_delay || BLOCKS.length < last_block + timeout_delay) {
        debug(`${label} send ${funcNum} --- post`);
        transfer(networkAccount, ctc, value);
        BLOCKS[BLOCKS.length - 1].event = { funcNum, from: address, data: args.slice(-1 * evt_cnt), value, balance: BALANCES[ctc.address] };
        return await recv(label, funcNum, evt_cnt, timeout_delay);
      } else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true };
      }
    };

    const recv = async (label, funcNum, evt_cnt, timeout_delay) => {
      let check_block = last_block;
      while (!timeout_delay || check_block < last_block + timeout_delay) {
        debug(`${label} recv ${funcNum} --- check ${check_block}`);
        const b = BLOCKS[check_block];
        if (!b || !b.event || b.event.funcNum != funcNum) {
          debug(`${label} recv ${funcNum} --- wait`);
          check_block = Math.min(check_block + 1, BLOCKS.length);
          await Timeout.set(1);
          continue;
        } else {
          debug(`${label} recv ${funcNum} --- recv`);
          last_block = check_block;
          const evt = b.event;
          return { didTimeout: false, data: evt.data, value: evt.value, balance: evt.balance, from: evt.from };
        }
      }

      debug(`${label} recv ${funcNum} --- timeout`);
      return { didTimeout: true };
    };

    return { ...ctc, sendrecv, recv, iam, wait };
  };

  const deploy = async (bin) => {
    const contract = makeAccount();
    debug(`new contract: ${contract.address}`);
    return await attach(bin, {
      ...contract,
      creation_block: BLOCKS.length,
      events: {},
    });
  };

  return { deploy, attach, networkAccount };
};

const makeAccount = () => {
  const address = stdlib.toHex(stdlib.randomUInt256());
  BALANCES[address] = 0;
  return { address };
};

export const newTestAccount = async (startingBalance) => {
  const acc = makeAccount();
  debug(`new account: ${acc.address}`);
  BALANCES[REACHY_RICH.address] = startingBalance;
  transfer(REACHY_RICH, acc, startingBalance);
  return await connectAccount(acc);
};

export function getNetworkTime() {
  return stdlib.bigNumberify(BLOCKS.length);
}

export function wait(delta, onProgress) {
  return waitUntilTime(stdlib.add(getNetworkTime(), delta), onProgress);
}

export function waitUntilTime(targetTime, onProgress) {
  targetTime = stdlib.bigNumberify(targetTime);
  onProgress = onProgress || (() => {});
  // FAKE is basically synchronous,
  // so it doesn't make sense to actually "wait" idly.
  let currentTime;
  while (stdlib.lt((currentTime = getNetworkTime()), targetTime)) {
    onProgress({ currentTime, targetTime });
    BLOCKS.push({ type: 'wait' });
  }
  // Also report progress at completion time
  onProgress({ currentTime, targetTime });
}

export const newAccountFromMnemonic = false; // XXX
export const verifyContract = false; // XXX
