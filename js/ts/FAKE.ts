import Timeout from 'await-timeout';
import ethers from 'ethers';

import * as stdlib from './shared';
import { CurrencyAmount, TyContract } from './shared';
export * from './shared';

export const debug = (msg: any): void => {
  stdlib.debug(`${BLOCKS.length}: ${msg}}`);
};

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;
export const UInt_max: BigNumber =
  BigNumber.from(2).pow(256).sub(1);

type Address = string;
type NetworkAccount = {address: Address};
type Backend = null;

// XXX Add optional "deploy after first message" info
type ContractInfo = {
  address: Address,
  creation_block: number,
}

type Digest = Array<any>;
type Recv = stdlib.IRecv<Address>
type RecvNoTimeout = stdlib.IRecvNoTimeout<Address>
type Contract = stdlib.IContract<ContractInfo, Digest, Address>;
type Account = stdlib.IAccount<NetworkAccount, Backend, Contract, ContractInfo>;
type AccountTransferrable = stdlib.IAccountTransferable<NetworkAccount>
type SimRes = stdlib.ISimRes<Digest, Address>;
type SimTxn = stdlib.ISimTxn<Address>;

type Event = {
  funcNum: number,
  from: Address,
  data: Array<any>,
  value: BigNumber,
  txns: Array<SimTxn>,
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

type ContractBlock = {
  type: 'contract',
  address: Address
}

type Block = TransferBlock | EventBlock | ContractBlock | WaitBlock;

// This can be exposed to the user for checking the trace of blocks
// for testing.
const BLOCKS: Array<Block> = [];
// key: Address, but ts doesn't like aliases here
const BALANCES: {[key: string]: BigNumber} = {};

const toAcct = (address: Address): AccountTransferrable => ({
  networkAccount: {address}
});

export const balanceOf = async (acc: Account) => {
  return BALANCES[acc.networkAccount.address];
};

/**
 * @description performs a transfer; no block created
 */
const transfer_ = (
  froma: Address,
  toa: Address,
  value: BigNumber,
  is_ctc?: boolean,
): void => {
  if (is_ctc) {
    debug('transfer_: contract is paying out to someone');
  }
  stdlib.assert(stdlib.le(value, BALANCES[froma]));
  debug(`transfer_ ${froma} -> ${toa} of ${value}`);
  BALANCES[toa] = stdlib.add(BALANCES[toa], value);
  BALANCES[froma] = stdlib.sub(BALANCES[froma], value);
}

/**
 * @description performs a transfer & creates a transfer block
 */
export const transfer = async (
  from: AccountTransferrable,
  to: AccountTransferrable,
  value: BigNumber
): Promise<void> => {
  const toa = to.networkAccount.address;
  const froma = from.networkAccount.address;
  transfer_(froma, toa, value);
  const block: TransferBlock = { type: 'transfer', to: toa, from: froma, value };
  debug(`transfer: ${JSON.stringify(block)}`);
  BLOCKS.push(block);
};

export const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const { address } = networkAccount;

  const attach = (
    bin: Backend,
    infoP: ContractInfo | Promise<ContractInfo>
  ): Contract => {
    void(bin);

    // state
    const {getLastBlock, setLastBlock} = (() => {
      let lastBlock: number | null = null;
      const setLastBlock = (n: number): void => {
        lastBlock = n;
      };
      const getLastBlock = async (): Promise<number> => {
        if (typeof lastBlock === 'number') { return lastBlock; }
        const info = await infoP;
        setLastBlock(info.creation_block);
        return info.creation_block;
      }
      return {getLastBlock, setLastBlock};
    })();

    const iam = (some_addr: Address): Address => {
      if (some_addr === address) {
        return address;
      } else {
        throw Error(`I should be ${some_addr}, but am ${address}`);
      }
    };

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      // Don't wait from current time, wait from last_block
      return waitUntilTime(stdlib.add(await getLastBlock(), delta));
    };

    const sendrecv = async (
      label: string, funcNum: number, evt_cnt: number, tys: Array<TyContract<any>>,
      args: Array<any>, value: BigNumber, out_tys: Array<TyContract<any>>,
      timeout_delay: BigNumber | false, sim_p: (fake: Recv) => SimRes,
    ): Promise<Recv> => {
      void(tys);

      stdlib.assert(args.length === tys.length, {
        expected: args.length,
        actual: tys.length,
        message: 'tys does not have expected length',
      });
      const data = args.slice(args.length - evt_cnt).map((v, i) => {
        return out_tys[i].munge(v);
      });
      const last_block = await getLastBlock();
      const ctcInfo = await infoP;
      if (!timeout_delay || stdlib.lt(BLOCKS.length, stdlib.add(last_block, timeout_delay))) {
        debug(`${label} send ${funcNum} --- post`);

        transfer({networkAccount}, toAcct(ctcInfo.address), value);
        const stubbedRecv: RecvNoTimeout = {
          didTimeout: false,
          data,
          value,
          from: address,
        }
        const {txns} = sim_p(stubbedRecv);

        // Instead of processing these atomically & rolling back on failure
        // it is just assumed that using FAKE means it is all in one JS thread.
        // (A failed transfer will crash the whole thing.)
        for (const txn of txns) {
          transfer_(ctcInfo.address, txn.to, txn.amt, true);
        }
        const transferBlock = BLOCKS[BLOCKS.length - 1];
        if (transferBlock.type !== 'transfer') { throw Error('impossible: intervening block'); }
        const event: Event = { ...stubbedRecv, funcNum, txns };
        const block: EventBlock = { ...transferBlock, type: 'event', event };
        debug(`sendrecv: transforming transfer block into event block: ${JSON.stringify(block)}`)
        BLOCKS[BLOCKS.length - 1] = block;

        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      } else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true };
      }
    };

    const recv = async (
      label: string, funcNum: number, ok_cnt: number, out_tys: Array<TyContract<any>>,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(ok_cnt);
      void(out_tys);

      const last_block = await getLastBlock();
      let check_block = last_block;
      while (!timeout_delay || stdlib.lt(check_block, stdlib.add(last_block, timeout_delay))) {
        debug(`${label} recv ${funcNum} --- check ${check_block}`);
        const b = BLOCKS[check_block];
        if (!b || b.type !== 'event' || !b.event || !stdlib.eq(b.event.funcNum, funcNum)) {
          debug(`${label} recv ${funcNum} --- wait`);
          check_block = Math.min(check_block + 1, BLOCKS.length);
          await Timeout.set(1);
          continue;
        } else {
          debug(`${label} recv ${funcNum} --- recv`);
          setLastBlock(check_block);
          const evt = b.event;
          return { didTimeout: false, data: evt.data, value: evt.value, from: evt.from };
        }
      }

      debug(`${label} recv ${funcNum} --- timeout`);
      return { didTimeout: true };
    };

    const getInfo = async () => await infoP;

    return { getInfo, sendrecv, recv, iam, wait };
  };

  const deploy = (bin: Backend): Contract => {
    const contract = makeAccount();
    debug(`new contract: ${contract.address}`);
    BLOCKS.push({type: 'contract', address: contract.address});
    return attach(bin, {
      ...contract,
      creation_block: BLOCKS.length - 1,
      // events: {},
    });
  };

  return { deploy, attach, networkAccount };
};

const makeAccount = (): NetworkAccount => {
  const address = stdlib.toHex(stdlib.randomUInt());
  BALANCES[address] = stdlib.bigNumberify(0);
  return { address };
};

const REACHY_RICH_P: Promise<Account> = (async () => {
  return await connectAccount({address: 'reachy_rich'});
})();

export async function getDefaultAccount(): Promise<Account> {
  return REACHY_RICH_P;
}

export const newTestAccount = async (startingBalance: BigNumber) => {
  const REACHY_RICH = await REACHY_RICH_P;
  const networkAccount = makeAccount();
  debug(`new account: ${networkAccount.address}`);
  BALANCES[REACHY_RICH.networkAccount.address] = startingBalance;
  transfer(REACHY_RICH, {networkAccount}, startingBalance);
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
export function parseCurrency(amt: CurrencyAmount): BigNumber {
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
export function formatCurrency(amt: BigNumber, decimals: number = 0): string {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  void(decimals); // There are no fractional quantities in FAKE
  return amt.toString();
}
