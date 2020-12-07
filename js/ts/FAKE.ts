import Timeout from 'await-timeout';
import ethers from 'ethers';

import * as sharedStdlib from './shared';
import { CurrencyAmount, OnProgress } from './shared';
import ethStdlib from './ETH';

const { T_Null, T_Bool, T_UInt, T_Bytes, T_Address, T_Digest, T_Object, T_Data, T_Array, T_Tuple, addressEq, digest } = ethStdlib;

let stdlib: any = {
  get stdlib(): any { return this; }};

const debug = (msg: any): void => {
  sharedStdlib.debug(`${BLOCKS.length}: ${msg}}`);
};

type BigNumber = ethers.BigNumber;
const BigNumber = ethers.BigNumber;
const UInt_max: BigNumber =
  BigNumber.from(2).pow(256).sub(1);
const { randomUInt, hasRandom } = sharedStdlib.makeRandom(32);

type Address = string;
type NetworkAccount = {address: Address};
type Backend = null;

type FAKE_Ty = any;

// XXX Add optional "deploy after first message" info
type ContractInfo = {
  address: Address,
  creation_block: number,
}

type Digest = string;
type Recv = sharedStdlib.IRecv<Address>
type RecvNoTimeout = sharedStdlib.IRecvNoTimeout<Address>
type Contract = sharedStdlib.IContract<ContractInfo, Digest, Address, FAKE_Ty>;
type Account = sharedStdlib.IAccount<NetworkAccount, Backend, Contract, ContractInfo>;
type AccountTransferrable = sharedStdlib.IAccountTransferable<NetworkAccount>
type SimRes = sharedStdlib.ISimRes<Digest, Address>;
type SimTxn = sharedStdlib.ISimTxn<Address>;

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

const balanceOf = async (acc: Account) => {
  return BALANCES[acc.networkAccount.address];
};

const STATES: {[key: string]: Digest} = {};
const checkStateTransition = async (which: string, prevSt: Digest, nextSt: Digest): Promise<boolean> => {
  const cur = STATES[which];
  debug(`cst ${JSON.stringify(prevSt)} on ${JSON.stringify(cur)} to ${JSON.stringify(nextSt)}`);
  await Timeout.set(Math.random() < 0.5 ? 20 : 0);
  if ( ! sharedStdlib.bytesEq(cur, prevSt) ) {
    return false;
  }
  STATES[which] = nextSt;
  return true; };

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
  sharedStdlib.assert(sharedStdlib.le(value, BALANCES[froma]));
  debug(`transfer_ ${froma} -> ${toa} of ${value}`);
  BALANCES[toa] = sharedStdlib.add(BALANCES[toa], value);
  BALANCES[froma] = sharedStdlib.sub(BALANCES[froma], value);
}

const fundFromFaucet = async (toa: AccountTransferrable, value: BigNumber) => {
  const faucet = await getFaucet();
  const faucetAddress = faucet.networkAccount.address;
  const faucetFunds = BALANCES[faucetAddress] || sharedStdlib.bigNumberify(0);
  // For FAKE, the faucet may need to add funds on demand,
  // if the user created an account without a starting balance.
  if (sharedStdlib.le(faucetFunds, value)) {
    BALANCES[faucetAddress] = faucetFunds.add(value);
  }
  transfer(faucet, toa, value);
}

/**
 * @description performs a transfer & creates a transfer block
 */
const transfer = async (
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

const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  const { address } = networkAccount;

  const stdlibT = stdlib;

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

    const selfAddress = (): Address => {
      return address;
    }

    const wait = async (delta: BigNumber): Promise<BigNumber> => {
      // Don't wait from current time, wait from last_block
      return waitUntilTime(sharedStdlib.add(await getLastBlock(), delta));
    };

    const sendrecv = async (
      label: string, funcNum: number, evt_cnt: number, tys: Array<FAKE_Ty>,
      args: Array<any>, value: BigNumber, out_tys: Array<FAKE_Ty>,
      timeout_delay: BigNumber | false, sim_p: (fake: Recv) => SimRes,
    ): Promise<Recv> => {
      void(tys);

      sharedStdlib.assert(args.length === tys.length, {
        expected: args.length,
        actual: tys.length,
        message: 'tys does not have expected length',
      });
      const data = sharedStdlib.argsSlice(args, evt_cnt);
      const last_block = await getLastBlock();
      const ctcInfo = await infoP;
      if (!timeout_delay || sharedStdlib.lt(BLOCKS.length, sharedStdlib.add(last_block, timeout_delay))) {
        debug(`${label} send ${funcNum} --- post`);

        const stubbedRecv: RecvNoTimeout = {
          didTimeout: false,
          data,
          value,
          from: address,
        }
        const {prevSt, nextSt, txns} = sim_p(stubbedRecv);
        if ( await checkStateTransition(ctcInfo.address, prevSt, nextSt) ) {
          transfer({networkAccount}, toAcct(ctcInfo.address), value);
          // Instead of processing these atomically & rolling back on failure
          // it is just assumed that using FAKE means it is all in one JS
          // thread.  (A failed transfer will crash the whole thing.)
          for (const txn of txns) {
            transfer_(ctcInfo.address, txn.to, txn.amt, true);
          }
          const theBlockNum = BLOCKS.length - 1;
          const transferBlock = BLOCKS[theBlockNum];
          if (transferBlock.type !== 'transfer') { throw Error(`impossible: intervening block ${JSON.stringify(BLOCKS)}`); }
          const event: Event = { ...stubbedRecv, funcNum, txns };
          const block: EventBlock = { ...transferBlock, type: 'event', event };
          debug(`sendrecv: ${theBlockNum} transforming transfer block into event block: ${JSON.stringify(block)}`)
          BLOCKS[theBlockNum] = block;
        }

        return await recv(label, funcNum, evt_cnt, out_tys, timeout_delay);
      } else {
        debug(`${label} send ${funcNum} --- timeout`);
        return { didTimeout: true };
      }
    };

    const recv = async (
      label: string, funcNum: number, ok_cnt: number, out_tys: Array<FAKE_Ty>,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(ok_cnt);
      void(out_tys);

      const last_block = await getLastBlock();
      let check_block = last_block;
      while (!timeout_delay || sharedStdlib.lt(check_block, sharedStdlib.add(last_block, timeout_delay))) {
        debug(`${label} recv ${funcNum} --- check ${check_block}`);
        const b = BLOCKS[check_block];
        if (!b || b.type !== 'event' || !b.event || !sharedStdlib.eq(b.event.funcNum, funcNum)) {
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

    return { getInfo, sendrecv, recv, iam, selfAddress, wait, stdlibT };
  };

  const deploy = (bin: Backend): Contract => {
    const contract = makeAccount();
    debug(`new contract: ${contract.address}`);
    STATES[contract.address] =
      // @ts-ignore XXX
      digest(T_Tuple([T_UInt]), [sharedStdlib.bigNumberify(0)]);
    BLOCKS.push({type: 'contract', address: contract.address});
    return attach(bin, {
      ...contract,
      creation_block: BLOCKS.length - 1,
      // events: {},
    });
  };

  return { deploy, attach, networkAccount, stdlibT };
};

const makeAccount = (): NetworkAccount => {
  const address = ethers.Wallet.createRandom().address;
  BALANCES[address] = sharedStdlib.bigNumberify(0);
  return { address };
};

const REACHY_RICH_P: Promise<Account> = (async () => {
  return await connectAccount({address: T_Address.defaultValue});
})();

async function getDefaultAccount(): Promise<Account> {
  return REACHY_RICH_P;
}

async function getFaucet(): Promise<Account> {
  return REACHY_RICH_P;
}

const newTestAccount = async (startingBalance: BigNumber) => {
  const account = await createAccount();
  debug(`new account: ${account.networkAccount.address}`);
  await fundFromFaucet(account, startingBalance);
  return account;
};

const createAccount = async () => {
  // Create account without any starting balance
  const networkAccount = makeAccount();
  debug(`createAccount: ${networkAccount.address}`);
  return await connectAccount(networkAccount);
}

function getNetworkTime() {
  return sharedStdlib.bigNumberify(BLOCKS.length);
}

function wait(delta: BigNumber | number, onProgress?: OnProgress): BigNumber {
  return waitUntilTime(sharedStdlib.add(getNetworkTime(), delta), onProgress);
}

function waitUntilTime(targetTime: BigNumber | number, onProgress?: OnProgress): BigNumber {
  targetTime = sharedStdlib.bigNumberify(targetTime);
  const onProg = onProgress || (() => {});
  // FAKE is basically synchronous,
  // so it doesn't make sense to actually "wait" idly.
  let currentTime;
  while (sharedStdlib.lt((currentTime = getNetworkTime()), targetTime)) {
    onProg({ currentTime, targetTime });
    BLOCKS.push({ type: 'wait', currentTime, targetTime });
  }
  // Also report progress at completion time
  onProg({ currentTime, targetTime });
  return currentTime;
}

const newAccountFromSecret = false; // XXX
const newAccountFromMnemonic = false; // XXX
const verifyContract = false; // XXX

/** @description the display name of the standard unit of currency for the network */
const standardUnit = 'FAKE';
/** @description the display name of the atomic (smallest) unit of currency for the network */
const atomicUnit = 'FAKE';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100'
 */
function parseCurrency(amt: CurrencyAmount): BigNumber {
  return sharedStdlib.bigNumberify(amt.toString());
}
const minimumBalance: BigNumber =
  parseCurrency(0);

/**
 * @description  Format currency by network
 * @param amt  the amount in the {@link atomicUnit} of the network.
 * @param decimals  up to how many decimal places to display in the {@link standardUnit}.
 *   Trailing zeroes will be omitted. Excess decimal places will be truncated. (not rounded)
 *   This argument defaults to maximum precision.
 * @returns  a string representation of that amount in the {@link standardUnit} for that network.
 * @example  formatCurrency(bigNumberify('100')); // => '100'
 */
function formatCurrency(amt: BigNumber, decimals: number = 0): string {
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  void(decimals); // There are no fractional quantities in FAKE
  return amt.toString();
}

const setFaucet = false; // XXX

stdlib = {
  ...stdlib,
  ...sharedStdlib,
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Digest,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple,
  addressEq,
  digest,
  UInt_max,
  randomUInt,
  hasRandom,
  balanceOf,
  transfer,
  connectAccount,
  newAccountFromSecret,
  newAccountFromMnemonic,
  getDefaultAccount,
  getFaucet,
  setFaucet,
  createAccount,
  fundFromFaucet,
  newTestAccount,
  getNetworkTime,
  wait,
  waitUntilTime,
  verifyContract,
  standardUnit,
  atomicUnit,
  parseCurrency,
  minimumBalance,
  formatCurrency,
}

export default stdlib;
