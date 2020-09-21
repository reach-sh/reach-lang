import Timeout from 'await-timeout';
import {BigNumber} from 'ethers';

import * as stdlib from './shared';
import { CurrencyMap, TyContract } from './shared';
export * from './shared';

const DEBUG = false;
const debug = (msg: any): void => {
  if (DEBUG) {
    console.log(`DEBUG@${BLOCKS.length}: ${msg}`);
  }
};

type Address = string;
type NetworkAccount = {address: Address};
type Account = {
  networkAccount: NetworkAccount,
  deploy?: (bin: Backend) => Promise<Contract>,
  attach?: (bin: Backend, ctc: Contract) => Promise<ContractAttached>,
};

type Backend = null;
type Contract = {
  address: Address,
  creation_block: number,
};

type ContractAttached = {
  address: Address,
  creation_block: number,
  sendrecv: (...xs: any) => any,
  recv: (...xs: any) => any,
  iam: (some_addr: Address) => Address,
  wait: (...xs: any) => any,
};

// TODO
type ContractOut = any;
// TODO: move common interfaces to shared
type Recv = {
  didTimeout: false,
  data: Array<ContractOut>,
  value: BigNumber,
  balance: BigNumber,
  from: Address,
} | { didTimeout: true };

const REACHY_RICH: NetworkAccount = {address: 'reachy_rich'};

type Event = {
  funcNum: number,
  from: Address,
  data: Array<any>,
  value: BigNumber,
  balance: BigNumber,
};

type TransferBlock = {
  type: 'transfer',
  to: Address,
  from: Address,
  value: BigNumber,
};

type EventBlock = {
  type: 'event',
  to: Address,
  from: Address,
  value: BigNumber,
  event: Event,
};

type WaitBlock = {
  type: 'wait',
  currentTime: BigNumber,
  targetTime: BigNumber,
};

type Block = TransferBlock | EventBlock | WaitBlock;

// This can be exposed to the user for checking the trace of blocks
// for testing.
const BLOCKS: Array<Block> = [];
// key: Address, but ts doesn't like aliases here
const BALANCES: {[key: string]: BigNumber} = {};

export const balanceOf = async (acc: Account) => {
  return BALANCES[acc.networkAccount.address];
};

export const transfer = async (from: Account, to: Account, value: BigNumber): Promise<void> => {
  const toa = to.networkAccount.address;
  const froma = from.networkAccount.address;
  stdlib.assert(stdlib.le(value, BALANCES[froma]));
  debug(`transfer ${froma} -> ${toa} of ${value}`);
  BLOCKS.push({ type: 'transfer', to: toa, from: froma, value });
  BALANCES[toa] = stdlib.add(BALANCES[toa], value);
  BALANCES[froma] = stdlib.sub(BALANCES[froma], value);
};

export const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const { address } = networkAccount;

  const attach = async (bin: Backend, ctc: Contract): Promise<ContractAttached> => {
    void(bin);
    let last_block = ctc.creation_block;

    const iam = (some_addr: Address): Address => {
      if (some_addr == address) {
        return address;
      } else {
        throw Error(`I should be ${some_addr}, but am ${address}`);
      }
    };

    const wait = (delta: BigNumber): void => {
      // Don't wait from current time, wait from last_block
      waitUntilTime(stdlib.add(last_block, delta));
    };

    const sendrecv = async (
      label: string, funcNum: number, evt_cnt: number, tys: Array<TyContract<any>>,
      args: Array<any>, value: BigNumber, out_tys: Array<TyContract<any>>,
      timeout_delay: undefined | number | BigNumber, try_p: any
    ): Promise<Recv> => {
      // XXX use try_p to figure out what transfers from the contract
      // to make, like in ALGO
      void(tys);
      void(try_p);
      timeout_delay = toNumberMay(timeout_delay);

      if (!timeout_delay || stdlib.lt(BLOCKS.length, stdlib.add(last_block, timeout_delay))) {
        debug(`${label} send ${funcNum} --- post`);
        transfer({networkAccount}, {networkAccount: ctc}, value);
        const transferBlock = BLOCKS[BLOCKS.length - 1];
        if (transferBlock.type !== 'transfer') { throw Error('impossible: intervening block'); }
        const event = { funcNum, from: address, data: args.slice(-1 * evt_cnt), value, balance: BALANCES[ctc.address] };
        BLOCKS[BLOCKS.length - 1] = { ...transferBlock, type: 'event', event };
        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      } else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true };
      }
    };

    const recv = async (
      label: string, funcNum: number, ok_cnt: number, out_tys: Array<TyContract<any>>,
      timeout_delay: number | BigNumber | undefined,
    ): Promise<Recv> => {
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

  const deploy = async (bin: Backend): Promise<Contract> => {
    const contract = makeAccount();
    debug(`new contract: ${contract.address}`);
    return await attach(bin, {
      ...contract,
      creation_block: BLOCKS.length,
      // events: {},
    });
  };

  return { deploy, attach, networkAccount };
};

const makeAccount = (): NetworkAccount => {
  const address = stdlib.toHex(stdlib.randomUInt256());
  BALANCES[address] = stdlib.bigNumberify(0);
  return { address };
};

export const newTestAccount = async (startingBalance: BigNumber) => {
  const networkAccount = makeAccount();
  debug(`new account: ${networkAccount.address}`);
  BALANCES[REACHY_RICH.address] = startingBalance;
  transfer({networkAccount: REACHY_RICH}, {networkAccount}, startingBalance);
  return await connectAccount(networkAccount);
};

export function getNetworkTime() {
  return stdlib.bigNumberify(BLOCKS.length);
}

type OnProgress = (obj: {currentTime: BigNumber, targetTime: BigNumber}) => void

export function wait(delta: BigNumber | number, onProgress?: OnProgress): BigNumber {
  return waitUntilTime(stdlib.add(getNetworkTime(), delta), onProgress);
}

export function waitUntilTime(targetTime: BigNumber | number, onProgress?: OnProgress): BigNumber {
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

const toNumberMay = (x: number | BigNumber | undefined) => {
  if (stdlib.isBigNumber(x)) {
    return x.toNumber();
  } else {
    return x;
  }
};

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'reachies';
/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'reachies';

/**
 * @description  Parse currency by network
 * @param cm  a currency map, keyed by network, values are the standard unit for that network.
 *   For stdlib/ETH, this map must include ETH. The unit is ethers.
 * @returns  the amount in the atomic unit of the network.
 *   For stdlib/ETH this is WEI.
 * @example  parseCurrency({FAKE: 100}).toString() // => '100'
 */
export function parseCurrency(cm: CurrencyMap): BigNumber {
  if (!cm.FAKE) { throw Error(`Expected FAKE in ${Object.keys(cm)}`); }
  return stdlib.bigNumberify(cm.FAKE.toString());
}

/**
 * @description  Format currency by network
 * @param amt  the amount in the atomic unit of the network.
 *   For stdlib/ETH this is WEI.
 * @param decimals  up to how many decimal places to display in the standard unit.
 *   Trailing zeroes will be omitted.
 *   For stdlib/ETH this must be an int from 0 to 18 inclusive, and defaults to 18.
 * @returns  a string representation of that amount in the standard unit for that network.
 *   For stdlib/ETH this is ethers.
 * @example  formatCurrency(bigNumberify('100')); // => '100'
 */
export function formatCurrency(amt: BigNumber, decimals: number = 0): string {
  if (!(Number.isInteger(decimals) && decimals === 0)) {
    throw Error(`Expected decimals to be the integer 0, but got ${decimals}.`);
  }
  return amt.toString();
}
