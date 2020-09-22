import Timeout from 'await-timeout';
import * as stdlib from './shared.mjs';
export * from './shared.mjs';
const DEBUG = false;
const debug = (msg) => {
  if (DEBUG) {
    console.log(`DEBUG@${BLOCKS.length}: ${msg}`);
  }
};
const REACHY_RICH = { address: 'reachy_rich' };
// This can be exposed to the user for checking the trace of blocks
// for testing.
const BLOCKS = [];
// key: Address, but ts doesn't like aliases here
const BALANCES = {};
export const balanceOf = async (acc) => {
  return BALANCES[acc.networkAccount.address];
};
export const transfer = async (from, to, value) => {
  const toa = to.networkAccount.address;
  const froma = from.networkAccount.address;
  stdlib.assert(stdlib.le(value, BALANCES[froma]));
  debug(`transfer ${froma} -> ${toa} of ${value}`);
  BLOCKS.push({ type: 'transfer', to: toa, from: froma, value });
  BALANCES[toa] = stdlib.add(BALANCES[toa], value);
  BALANCES[froma] = stdlib.sub(BALANCES[froma], value);
};
export const connectAccount = async (networkAccount) => {
  const { address } = networkAccount;
  const attach = async (bin, ctc) => {
    void(bin);
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
    const sendrecv = async (label, funcNum, evt_cnt, tys, args, value, out_tys, timeout_delay, try_p) => {
      // XXX use try_p to figure out what transfers from the contract
      // to make, like in ALGO
      void(tys);
      void(try_p);
      timeout_delay = toNumberMay(timeout_delay);
      if (!timeout_delay || stdlib.lt(BLOCKS.length, stdlib.add(last_block, timeout_delay))) {
        debug(`${label} send ${funcNum} --- post`);
        transfer({ networkAccount }, { networkAccount: ctc }, value);
        const transferBlock = BLOCKS[BLOCKS.length - 1];
        if (transferBlock.type !== 'transfer') {
          throw Error('impossible: intervening block');
        }
        const event = { funcNum, from: address, data: args.slice(-1 * evt_cnt), value, balance: BALANCES[ctc.address] };
        BLOCKS[BLOCKS.length - 1] = { ...transferBlock, type: 'event', event };
        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      } else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true };
      }
    };
    const recv = async (label, funcNum, ok_cnt, out_tys, timeout_delay) => {
      void(ok_cnt);
      void(out_tys);
      timeout_delay = toNumberMay(timeout_delay);
      let check_block = last_block;
      while (!timeout_delay || stdlib.lt(check_block, stdlib.add(last_block, timeout_delay))) {
        debug(`${label} recv ${funcNum} --- check ${check_block}`);
        const b = BLOCKS[check_block];
        if (!b || b.type !== 'event' || !b.event || b.event.funcNum != funcNum) {
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
    });
  };
  return { deploy, attach, networkAccount };
};
const makeAccount = () => {
  const address = stdlib.toHex(stdlib.randomUInt256());
  BALANCES[address] = stdlib.bigNumberify(0);
  return { address };
};
export const newTestAccount = async (startingBalance) => {
  const networkAccount = makeAccount();
  debug(`new account: ${networkAccount.address}`);
  BALANCES[REACHY_RICH.address] = startingBalance;
  transfer({ networkAccount: REACHY_RICH }, { networkAccount }, startingBalance);
  return await connectAccount(networkAccount);
};
export function getNetworkTime() {
  return stdlib.bigNumberify(BLOCKS.length);
}
export function wait(delta, onProgress) {
  return waitUntilTime(stdlib.add(getNetworkTime(), delta), onProgress);
}
export function waitUntilTime(targetTime, onProgress) {
  targetTime = stdlib.bigNumberify(targetTime);
  const onProg = onProgress || (() => {});
  // FAKE is basically synchronous,
  // so it doesn't make sense to actually "wait" idly.
  let currentTime;
  while (stdlib.lt((currentTime = getNetworkTime()), targetTime)) {
    onProg({ currentTime, targetTime });
    BLOCKS.push({ type: 'wait', currentTime, targetTime });
  }
  // Also report progress at completion time
  onProg({ currentTime, targetTime });
  return currentTime;
}
export const newAccountFromMnemonic = false; // XXX
export const verifyContract = false; // XXX
const toNumberMay = (x) => {
  if (stdlib.isBigNumber(x)) {
    return x.toNumber();
  } else {
    return x;
  }
};
/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'FAKE';
/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'FAKE';
/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100'
 */
export function parseCurrency(amt) {
  return stdlib.bigNumberify(amt.toString());
}
/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100')); // => '100'
 */
export function formatCurrency(amt, decimals = 0) {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  void(decimals); // There are no fractional quantities in FAKE
  return amt.toString();
}
