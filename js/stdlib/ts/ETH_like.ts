import Timeout from 'await-timeout';
import { ethers as real_ethers } from 'ethers';
import {
  assert,
  eq,
} from './shared_backend';
import {
  replaceableThunk,
  debug,
  getViewsHelper,
  deferContract,
  makeRandom,
  argsSplit,
  ensureConnectorAvailable,
  make_newTestAccounts,
  make_waitUntilX,
  checkTimeout,
} from './shared_impl';
import {
  bigNumberify,
  bigNumberToNumber,
} from './shared_user';
import ETHstdlib from './stdlib_sol';

// Types-only imports
import type { // =>
  BigNumber } from 'ethers';
import type { // =>
  CurrencyAmount,
  IAccount,
  IBackend,
  IViewLib,
  IBackendViewInfo,
  IBackendViewsInfo,
  IContract,
  IRecvArgs, ISendRecvArgs,
  IRecv,
  TimeArg,
  OnProgress,
} from './shared_impl';
import type { // =>
  AnyETH_Ty,
  Token,
  PayAmt,
} from './ETH_like_compiled';
import type { // =>
  EthersLikeContract,
  EthersLikeSigner,
  EthersLikeWallet,
  EthLikeArgs,
  // EthLike, // TODO: use this once types are in place
} from './ETH_like_interfaces';
import type { // =>
  Stdlib_Backend
} from './interfaces';

// ****************************************************************************
// Type Definitions
// ****************************************************************************

// XXX make interfaces for these
type TransactionReceipt = real_ethers.providers.TransactionReceipt;
type Log = real_ethers.providers.Log;

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

const reachBackendVersion = 2;
const reachEthBackendVersion = 2;
type Backend = IBackend<AnyETH_Ty> & {_Connectors: {ETH: {
  version: number,
  ABI: string,
  Bytecode: string,
  views: {[viewn: string]: {[keyn: string]: string}},
}}};
type BackendViewsInfo = IBackendViewsInfo<AnyETH_Ty>;
type BackendViewInfo = IBackendViewInfo<AnyETH_Ty>;

// TODO: a wrapper obj with smart constructor?
type Address = string;

type NetworkAccount = {
  address?: Address, // required for receivers & deployers
  getAddress?: () => Promise<Address>, // or this for receivers & deployers
  sendTransaction?: (...xs: any) => any, // required for senders
  getBalance?: (...xs: any) => any, // TODO: better type
} | EthersLikeWallet | EthersLikeSigner; // required to deploy/attach

type ContractInfo = Address;
type SendRecvArgs = ISendRecvArgs<Address, Token, AnyETH_Ty>;
type RecvArgs = IRecvArgs<AnyETH_Ty>;
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Address, Token, AnyETH_Ty>;
export type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
  | any /* union in this field: { setGasLimit: (ngl:any) => void } */;

// For when you init the contract with the 1st message
type ContractInitInfo = {
  arg: any,
  value: BigNumber,
};

type AccountTransferable = Account | {
  networkAccount: NetworkAccount,
};

// ****************************************************************************
// Helpers
// ****************************************************************************

// Given a func that takes an optional arg, and a Maybe arg:
// f: (arg?: X) => Y
// arg: Maybe<X>
//
// You can apply the function like this:
// f(...argMay)
type Some<T> = [T];
type None = [];
type Maybe<T> = None | Some<T>;
function isNone<T>(m: Maybe<T>): m is None {
  return m.length === 0;
}
function isSome<T>(m: Maybe<T>): m is Some<T> {
  return !isNone(m);
}
const Some = <T>(m: T): Some<T> => [m];
const None: None = [];
void(isSome);

// TODO: add return type once types are in place
export function makeEthLike(ethLikeArgs: EthLikeArgs) {
// ...............................................
const {
  ethLikeCompiled,
  ethers,
  standardDigits = 18,
  providerLib,
  isIsolatedNetwork,
  canGetDefaultAccount,
  // isWindowProvider,
  _getDefaultNetworkAccount,
  _getDefaultFaucetNetworkAccount,
  _warnTxNoBlockNumber = true,
  _specialFundFromFaucet = async () => null,
  canFundFromFaucet,
  standardUnit,
  atomicUnit,
  validQueryWindow,
} = ethLikeArgs;

const {
  getProvider
} = providerLib;
const {
  stdlib,
} = ethLikeCompiled;
const {
  T_Address, T_Tuple,
  T_UInt,
  addressEq,
} = stdlib;
const reachStdlib: Stdlib_Backend<AnyETH_Ty> = stdlib;

const [_getQueryLowerBound, _setQueryLowerBound] = replaceableThunk(() => 0);

function getQueryLowerBound() {
  return bigNumberify(_getQueryLowerBound());
}

function setQueryLowerBound(networkTime: BigNumber|number) {
  networkTime = typeof networkTime === 'number' ? networkTime
    : networkTime._isBigNumber ? networkTime.toNumber()
    : networkTime;
  if (!(typeof networkTime === 'number')) { throw Error(`Expected number or BigNumber, but got ${networkTime} : ${typeof networkTime}`);}
  _setQueryLowerBound(networkTime);
}

/** @description convenience function for drilling down to the actual address */
const getAddr = async (acc: AccountTransferable): Promise<Address> => {
  if (!acc.networkAccount) throw Error(`Expected acc.networkAccount`);
  // TODO better type design here
  // @ts-ignore
  if (acc.networkAccount.address) {
    // @ts-ignore
    return acc.networkAccount.address;
  }
  if (acc.networkAccount.getAddress) {
    return await acc.networkAccount.getAddress();
  }
  throw Error(`Expected acc.networkAccount.address or acc.networkAccount.getAddress`);
}

// Helpers for sendrecv and recv

type Hash = string;

const rejectInvalidReceiptFor = async (txHash: Hash, r: TransactionReceipt|undefined): Promise<TransactionReceipt> =>
  new Promise((resolve, reject) =>
    !r ? reject(`No receipt for txHash: ${txHash}`) :
    r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`) :
    !r.status ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`) :
    resolve(r));

const fetchAndRejectInvalidReceiptFor = async (txHash: Hash) => {
  const provider = await getProvider();
  const r = await provider.getTransactionReceipt(txHash);
  return await rejectInvalidReceiptFor(txHash, r);
};

const getNetworkTimeNumber = async (): Promise<number> => {
  const provider = await getProvider();
  const ans = await provider.getBlockNumber();
  return ans;
};

const sendRecv_prepArg = (args:Array<any>, tys:Array<any>, evt_cnt:number) => {
  const [ args_svs, args_msg ] = argsSplit(args, evt_cnt);
  const [ tys_svs, tys_msg ] = argsSplit(tys, evt_cnt);
  // @ts-ignore XXX
  const arg_ty = T_Tuple([T_Tuple(tys_svs), T_Tuple(tys_msg)]);
  return arg_ty.munge([args_svs, args_msg]);
};

const initOrDefaultArgs = (init?: ContractInitInfo): ContractInitInfo => ({
  arg: init ? init.arg : sendRecv_prepArg([], [], 0),
  value: init ? init.value : bigNumberify(0),
});

// ****************************************************************************
// Event Cache
// ****************************************************************************

const getMinBlock = (logs: any[]) =>
  logs.reduce((acc: Log, x: Log) =>
    (x.blockNumber == acc.blockNumber)
      ? (x.logIndex < acc.logIndex ? x : acc)
      : (x.blockNumber.toString() < acc.blockNumber.toString() ? x : acc), logs[0]);

const getMaxBlock = (logs: any[]) =>
  logs.reduce((acc: Log, x: Log) =>
    (x.blockNumber == acc.blockNumber)
      ? (x.logIndex > acc.logIndex ? x : acc)
      : (x.blockNumber.toString() > acc.blockNumber.toString() ? x : acc), logs[0]);

type QueryResult =
  | { succ: true, evt: any }
  | { succ: false, block: number }

class EventCache {
  cache: any[] = [];

  theAddress: string|undefined;
  public currentBlock: number;
  lastQueryTime: number = 0;

  constructor() {
    this.currentBlock = _getQueryLowerBound();
    this.cache = [];
    this.theAddress = undefined;
  }

  checkAddress(address: string) {
    if ( this.theAddress !== undefined ) {
      assert(address == this.theAddress, `address must match: ${address} != ${this.theAddress}`);
    } else {
      this.theAddress = address;
    }
  }

  async query(dhead:any, getC: () => Promise<EthersLikeContract>, fromBlock: number, timeoutAt: TimeArg | undefined, evt: string ) {
    const ethersC = await getC();
    return await this.queryContract(dhead, ethersC.address, ethersC.interface, fromBlock, timeoutAt, evt);
  }

  async queryContract(dhead:any, address:string, iface:any, fromBlock: number, timeoutAt: TimeArg|undefined, evt: string) {
    const topic = iface.getEventTopic(evt);
    this.checkAddress(address);
    return await this.query_(dhead, fromBlock, timeoutAt, topic);
  }

  async query_(dhead:any, fromBlock: number, timeoutAt: TimeArg|undefined, topic: string): Promise<QueryResult> {
    const lab = `EventCache.query`;
    debug(dhead, lab, { fromBlock, timeoutAt, topic });

    const h = (mode:string): (number | undefined) => timeoutAt && timeoutAt[0] === mode ? bigNumberToNumber(timeoutAt[1]) : undefined;
    const maxTime = h('time');
    const maxSecs = h('secs');
    debug(dhead, lab, { maxTime, maxSecs });

    // Clear cache of stale transactions
    // Cache's min bound will be `fromBlock`
    const showCache = (when:string) => {
      debug(dhead, lab, { when, current: this.currentBlock, len: this.cache.length});
    };
    showCache(`pre from`);
    this.cache = this.cache.filter((x) => x.blockNumber >= fromBlock);
    showCache(`post from`);

    // Search for target
    const searchLogs = async (source: any): Promise<any[]> => {
      const res = [];
      for ( const x of source ) {
        const block = x.blockNumber;
        if ( x.topics.includes(topic.toString())
            && (maxTime ? block <= maxTime : true)
            && (maxSecs ? (await getTimeSecs(block)).lte(maxSecs) : true) ) {
            res.push(x);
        }
      }
      return res;
    };

    const initLogs = await searchLogs(this.cache);
    if(initLogs.length > 0) {
      debug(dhead, lab, `in cache`);
      return { succ: true, evt: getMinBlock(initLogs) };
    }
    debug(dhead, lab, `not in cache`);

    // If no results, then contact network
    const failed = (): {succ: false, block: number} => ({ succ: false, block: this.currentBlock });

    debug(dhead, lab, `querying`);

    const leftOver = this.lastQueryTime + 1000 - Date.now();
    if ( leftOver > 0 ) {
      debug(dhead, lab, `waiting...`, leftOver);
      await Timeout.set(leftOver);
    }
    this.lastQueryTime = Date.now();

    const provider = await getProvider();
    const fromBlock_act = Math.max(fromBlock, this.currentBlock);
    const currentTime = await getNetworkTimeNumber();
    debug(dhead, lab, { fromBlock_act, currentTime });
    if ( fromBlock_act > currentTime ) {
      debug(dhead, lab, `no contact, from block in future`);
      return failed();
    }

    const toBlock =
      validQueryWindow === true
      ? currentTime
      : Math.min(currentTime, fromBlock_act + validQueryWindow);
    debug(dhead, lab, { fromBlock_act, currentTime, toBlock });
    assert(fromBlock <= toBlock, "from <= to");

    let res = [];
    try {
      res = await provider.getLogs({
        fromBlock: fromBlock_act,
        toBlock,
        address: this.theAddress
      });
    } catch (e) {
      debug(dhead, lab, 'getLogs err', e);
      return failed();
    }

    debug(dhead, lab, 'getLogs succ', res);
    this.cache = res;
    this.currentBlock =
      (this.cache.length == 0)
        ? toBlock
        : getMaxBlock(this.cache).blockNumber;
    debug(dhead, lab, 'got network', this.currentBlock);

    // Check for pred again
    const foundLogs = await searchLogs(this.cache);
    if ( foundLogs.length > 0 ) {
      debug(dhead, lab, `in network`);
      return { succ: true, evt: getMinBlock(foundLogs) };
    }

    debug(dhead, lab, `not in network`);
    return failed();
  }
}

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

const { randomUInt, hasRandom } = makeRandom(32);

const balanceOf = async (acc: Account, token: Token|false = false): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) {
    throw Error(`acc.networkAccount missing. Got: ${acc}`);
  }
  if ( ! token && networkAccount.getBalance ) {
    return bigNumberify(await networkAccount.getBalance());
  }
  const addr = await getAddr(acc);
  if (! addr) {
    throw Error(`address missing. Got: ${networkAccount}`);
  }

  if ( ! token ) {
    const provider = await getProvider();
    return bigNumberify(await provider.getBalance(addr));
  } else {
    return await balanceOf_token(networkAccount, addr, token);
  }
};

const ReachToken_ABI = ETHstdlib["contracts"]["stdlib.sol:ReachToken"]["abi"];
const ERC20_ABI = ETHstdlib["contracts"]["stdlib.sol:IERC20"]["abi"];

const balanceOf_token = async (networkAccount: NetworkAccount, address: Address, tok: Token): Promise<BigNumber> => {
  // @ts-ignore
  const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
  return bigNumberify(await tokCtc["balanceOf"](address));
};

const doTxn = async (
  dhead: any,
  tp: Promise<any>,
): Promise<void> => {
  debug({...dhead, step: `pre call`});
  const rt = await tp;
  debug({...dhead, rt, step: `pre wait`});
  const rm = await rt.wait();
  debug({...dhead, rt, rm, step: `pre receipt`});
  assert(rm !== null, `receipt wait null`);
  const ro = await fetchAndRejectInvalidReceiptFor(rm.transactionHash);
  debug({...dhead, rt, rm, ro, step: `post receipt`});
  // ro's blockNumber might be interesting
  void(ro);
};

const doCall = async (
  dhead: any,
  ctc: EthersLikeContract,
  funcName: string,
  args: Array<any>,
  value: BigNumber,
  gasLimit: BigNumber|undefined,
  storageLimit: BigNumber|undefined,
): Promise<void> => {
  const dpre = { ...dhead, funcName, args, value };
  debug({...dpre, step: `pre call`});
  let tx: any = { value, gasLimit };
  if (storageLimit !== undefined) { tx = { ...tx, storageLimit }; }
  return await doTxn(
    dpre,
    ctc[funcName](...args, tx));
};

/** @description Arg order follows "src before dst" convention */
const transfer = async (
  from: AccountTransferable,
  to: AccountTransferable,
  value: any,
  token: Token|false = false,
): Promise<any> => {
  const sender = from.networkAccount;
  const receiver = await getAddr(to);
  const valueb = bigNumberify(value);

  const dhead = {kind:'transfer'};
  if ( ! token ) {
    const txn = { to: receiver, value: valueb };
    debug('sender.sendTransaction(', txn, ')');
    return await doTxn(dhead, sender.sendTransaction(txn));
  } else {
    const tokCtc = new ethers.Contract(token, ERC20_ABI, sender);
    const gl = from.getGasLimit ? from.getGasLimit() : undefined;
    const sl = from.getStorageLimit ? from.getStorageLimit() : undefined;
    return await doCall(dhead, tokCtc, "transfer", [receiver, valueb], bigNumberify(0), gl, sl);
  }
};

const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  // @ts-ignore // TODO
  if (networkAccount.getAddress && !networkAccount.address) {
    // @ts-ignore
    networkAccount.address = await getAddr({networkAccount});
  }

  // XXX networkAccount MUST be a Wallet or Signer to deploy/attach
  const address = await getAddr({networkAccount});
  if (!address) { throw Error(`Expected networkAccount.address: ${networkAccount}`); }
  const shad = address.substring(2, 6);
  let label = shad;

  const iam = (some_addr: Address): Address => {
    if (addressEq(some_addr, address)) {
      return address;
    } else {
      throw Error(`I should be ${some_addr}, but am ${address}`);
    }
  };

  const selfAddress = (): Address => {
    return address;
  }

  let gasLimit: BigNumber;
  const setGasLimit = (ngl:any): void => {
    gasLimit = bigNumberify(ngl); };
  const getGasLimit = (): BigNumber => gasLimit;

  let storageLimit: BigNumber;
  const setStorageLimit = (bn: any): void => {
    storageLimit = bigNumberify(bn);
  }
  const getStorageLimit = (): BigNumber => storageLimit;

  const deploy = (bin: Backend): Contract => {
    ensureConnectorAvailable(bin, 'ETH', reachBackendVersion, reachEthBackendVersion);

    if (!ethers.Signer.isSigner(networkAccount)) {
      throw Error(`Signer required to deploy, ${networkAccount}`);
    }

    const {infoP, resolveInfo} = (() => {
      let resolveInfo = (info: ContractInfo) => { void(info); };
      const infoP = new Promise<ContractInfo>(resolve => {
        resolveInfo = resolve;
      });
      return {infoP, resolveInfo};
    })();

    const performDeploy = (
      init?: ContractInitInfo
    ): Contract => {
      debug(shad, ': performDeploy with', init);
      const { arg, value } = initOrDefaultArgs(init);
      debug(shad, {arg});

      const { ABI, Bytecode } = bin._Connectors.ETH;
      debug(shad, ': making contract factory');
      const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);

      (async () => {
        debug(shad, `: deploying factory`);
        const contract = await factory.deploy(arg, { value, gasLimit });
        debug(shad, `: deploying factory; done:`, contract.address);
        debug(shad, `: waiting for receipt:`, contract.deployTransaction.hash);
        const deploy_r = await contract.deployTransaction.wait();
        debug(shad, `: got receipt;`, deploy_r.blockNumber);
        const info: ContractInfo = contract.address;
        // XXX creation_block: deploy_r.blockNumber,
        // XXX transactionHash: deploy_r.transactionHash,
        resolveInfo(info);
      })();

      return attach(bin, infoP);
    };

    const attachDeferDeploy = (): Contract => {
      let setImpl:any;
      const implP: Promise<Contract> =
        new Promise((resolve) => { setImpl = resolve; });
      const implNow = {
        stdlib,
        sendrecv: async (srargs:SendRecvArgs): Promise<Recv> => {
          const { funcNum, evt_cnt, tys, out_tys, args, pay, onlyIf, soloSend, timeoutAt } = srargs;
          debug(shad, `:`, label, 'sendrecv m', funcNum, `(deferred deploy)`);
          const [ value, toks ] = pay;

          // The following must be true for the first sendrecv.
          try {
            assert(onlyIf, `firstMsg: onlyIf must be true`);
            assert(soloSend, `firstMsg: soloSend must be true`);
            assert(eq(funcNum, 0), `firstMsg: funcNum must be 1`);
            assert(!timeoutAt, `firstMsg: no timeout`);
            assert(toks.length == 0, `firstMsg: no tokens`);
          } catch (e) {
            throw Error(`impossible: Deferred deploy sendrecv assumptions violated.\n${e}`);
          }

          // shim impl is replaced with real impl
          setImpl(performDeploy({arg: sendRecv_prepArg(args, tys, evt_cnt), value}));
          await infoP; // Wait for the deploy to actually happen.

          // simulated recv
          return await impl.recv({funcNum, evt_cnt, out_tys, waitIfNotPresent: false, timeoutAt});
        },
      };
      const impl: Contract = deferContract(true, implP, implNow);
      return impl;
    }

    const deployMode = bin._deployMode;
    switch (deployMode) {
      case 'DM_firstMsg':
        return attachDeferDeploy();
      case 'DM_constructor':
        return performDeploy();
      default:
        throw Error(`Unrecognized deployMode: ${deployMode}`);
    };
  };

  const attach = (
    bin: Backend,
    infoP: Promise<ContractInfo>,
  ): Contract => {
    ensureConnectorAvailable(bin, 'ETH', reachBackendVersion, reachEthBackendVersion);

    const eventCache = new EventCache();

    const ABI = JSON.parse(bin._Connectors.ETH.ABI);

    // Attached state
    const {getLastBlock, setLastBlock} = (() => {
      let lastBlock: number | null = null;
      const setLastBlock = (n: number): void => {
        if (typeof n !== 'number') { throw Error(`Expected lastBlock number, got ${lastBlock}: ${typeof lastBlock}`) }
        debug(`lastBlock from`, lastBlock, `to`, n);
        lastBlock = n;
      };
      const getLastBlock = async (): Promise<number> => {
        if (typeof lastBlock === 'number') { return lastBlock; }
        // This causes lastBlock to be set
        await getC();
        return await getLastBlock();
      }
      return {getLastBlock, setLastBlock};
    })();

    const updateLast = (o: {blockNumber?: number}): void => {
      if (!o.blockNumber) {
        console.log(o);
        throw Error(`Expected blockNumber in ${Object.keys(o)}`);
      }
      setLastBlock(o.blockNumber);
    };

    const getC = (() => {
      let _ethersC: EthersLikeContract | null = null;
      return async (): Promise<EthersLikeContract> => {
        if (_ethersC) { return _ethersC; }
        const info = await infoP;
        const { creation_block } = await verifyContract_(info, bin, eventCache, label);
        setLastBlock(creation_block);
        const address = info;
        debug(shad, `: contract verified`);
        if (!ethers.Signer.isSigner(networkAccount)) {
          throw Error(`networkAccount must be a Signer (read: Wallet). ${networkAccount}`);
        }
        // TODO: remove "as" when we figure out how to type the interface for ctors
        _ethersC = new ethers.Contract(address, ABI, networkAccount) as EthersLikeContract;
        return _ethersC;
      }
    })();

    const callC = async (
      dhead: any, funcName: string, arg: any, pay: PayAmt,
    ): Promise<void> => {
      const [ value, toks ] = pay;
      const ethersC = await getC();
      const zero = bigNumberify(0);
      const actualCall = async () =>
        await doCall({...dhead, kind:'reach'}, ethersC, funcName, [arg], value, gasLimit, storageLimit);
      const callTok = async (tok:Token, amt:BigNumber) => {
        const tokBalance = await balanceOf_token(networkAccount, address, tok);
        debug({...dhead, kind:'token'}, 'balanceOf', tokBalance);
        assert(tokBalance.gte(amt), `local account token balance is insufficient: ${tokBalance} < ${amt}`);
        // @ts-ignore
        const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
        await doCall({...dhead, kind:'token'}, tokCtc, "approve", [ethersC.address, amt], zero, gasLimit, storageLimit); }
      const maybePayTok = async (i:number) => {
        if ( i < toks.length ) {
          const [amt, tok] = toks[i];
          await callTok(tok, amt);
          try {
            await maybePayTok(i+1);
          } catch (e) {
            await callTok(tok, zero);
            throw e;
          }
        } else {
          await actualCall();
        }
      };
      await maybePayTok(0);
    };

    const getEventData = async (
      ok_evt: string, ok_e: Log
    ): Promise<Array<any>> => {
      const ethersC = await getC();
      const ok_args_abi = ethersC.interface.getEvent(ok_evt).inputs;
      const { args } = ethersC.interface.parseLog(ok_e);
      return ok_args_abi.map(a => args[a.name]);
    };

    const getLog = async (
      fromBlock: number, toBlock: number, ok_evt: string,
    ): Promise<Log|undefined> => {
      const res = await eventCache.query('getLog', getC, fromBlock, ['time', bigNumberify(toBlock)], ok_evt);
      if ( ! res.succ ) { return undefined; }
      return res.evt;
    }

    const getInfo = async () => await infoP;

    const sendrecv = async (srargs:SendRecvArgs): Promise<Recv> => {
      const { funcNum, evt_cnt, tys, args, pay, out_tys, onlyIf, soloSend, timeoutAt } = srargs;
      const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
        await recv({funcNum, evt_cnt, out_tys, waitIfNotPresent, timeoutAt});
      if ( ! onlyIf ) {
        return await doRecv(true);
      }

      const funcName = `m${funcNum}`;
      if (tys.length !== args.length) {
        throw Error(`tys.length (${tys.length}) !== args.length (${args.length})`);
      }

      const dhead = [shad, label, 'send', funcName, timeoutAt, 'SEND'];
      debug(...dhead, 'ARGS', args);
      const arg = sendRecv_prepArg(args, tys, evt_cnt);

      // Make sure the ctc is available and verified (before we get into try/catch)
      // https://github.com/reach-sh/reach-lang/issues/134
      await getC();

      debug(...dhead, 'START', arg);
      const lastBlock = await getLastBlock();
      let block_send_attempt = lastBlock;
      let block_repeat_count = 0;
      while ( ! await checkTimeout(getTimeSecs, timeoutAt, block_send_attempt) ) {
        debug(...dhead, 'TRY');
        try {
          debug(...dhead, 'ARG', arg, pay);
          await callC(dhead, funcName, arg, pay);
        } catch (e) {
          if ( ! soloSend ) {
            debug(...dhead, `SKIPPING`, e);
          } else {
            debug(...dhead, `ERROR`, { stack: e.stack });

            // XXX What should we do...? If we fail, but there's no timeout delay... then we should just die
            await Timeout.set(1);
            const current_block = await getNetworkTimeNumber();
            if (current_block == block_send_attempt) {
              block_repeat_count++;
            }
            block_send_attempt = current_block;
            if ( block_repeat_count > 32) {
              if (e.code === 'UNPREDICTABLE_GAS_LIMIT') {
                let error = e;
                while (error.error) { error = error.error; }
                console.log(`impossible: The message you are trying to send appears to be invalid.`);
                console.log(error);
              }
              console.log(`args:`);
              console.log(arg);
              throw Error(`${dhead} REPEAT @ ${block_send_attempt} x ${block_repeat_count}`);
            }
            debug(...dhead, `TRY FAIL`, lastBlock, current_block, block_repeat_count, block_send_attempt);
            continue;
          }
        }

        debug(...dhead, 'SUCC');
        return await doRecv(false);
      }

      // XXX If we were trying to join, but we got sniped, then we'll
      // think that there is a timeout and then we'll wait forever for
      // the timeout message.

      debug(...dhead, `FAIL/TIMEOUT`);
      return {didTimeout: true};
    };

    // https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
    const recv = async (rargs:RecvArgs): Promise<Recv> => {
      const { funcNum, out_tys, waitIfNotPresent, timeoutAt } = rargs;
      const isCtor = (funcNum == 0)
      const lastBlock = await getLastBlock();
      const ok_evt = `e${funcNum}`;
      const dhead = { t: 'recv', label, ok_evt };
      debug(dhead, `START`);

      // look after the last block
      const fromBlock: number =
        lastBlock + (isCtor ? 0 : 1);
      while ( true ) {
        const res = await eventCache.query(dhead, getC, fromBlock, timeoutAt, ok_evt);
        if ( ! res.succ ) {
          const currentTime = res.block;
          if ( await checkTimeout(getTimeSecs, timeoutAt, currentTime) ) {
            debug(dhead, '--- RECVD timeout', {timeoutAt, currentTime});
            return { didTimeout: true };
          }
          if ( waitIfNotPresent ) {
            await waitUntilTime(bigNumberify(currentTime + 1));
          } else {
            // Ideally we'd wait until after time has advanced
            await Timeout.set(500);
          }
          continue;
        } else {
          const ok_e = res.evt;
          debug(dhead, `OKAY`);

          const ok_r = await fetchAndRejectInvalidReceiptFor(ok_e.transactionHash);
          debug(dhead, 'ok_r', ok_r);
          const ok_t = await (await getProvider()).getTransaction(ok_e.transactionHash);
          debug(dhead, 'ok_t', ok_t);

          // The .gas field doesn't exist on this anymore, apparently?
          // debug(`${ok_evt} gas was ${ok_t.gas} ${ok_t.gasPrice}`);

          if (ok_t.blockNumber) {
            assert(ok_t.blockNumber == ok_r.blockNumber,
              'recept & transaction block numbers should match');
            if (ok_e.blockNumber) {
              assert(ok_t.blockNumber == ok_e.blockNumber,
                'event & transaction block numbers should match');
            }
          } else {
            // XXX For some reason ok_t sometimes doesn't have blockNumber
            if (_warnTxNoBlockNumber) {
              console.log(`WARNING: no blockNumber on transaction.`);
              console.log(ok_t);
            }
          }

          const theBlock = ok_r.blockNumber;
          debug(dhead, `AT`, theBlock);
          updateLast(ok_r);
          const ok_ed = await getEventData(ok_evt, ok_e);
          debug(dhead, `DATA`, ok_ed);
          const ok_vals = ok_ed[0][1];
          debug(dhead, `MSG`, ok_vals);
          const data = T_Tuple(out_tys).unmunge(ok_vals) as unknown[]; // TODO: typing

          const _getLog = async (l_evt:string, l_ctc:any): Promise<any> => {
            let dheadl = [ dhead, 'getLog', l_evt, l_ctc];
            debug(dheadl);
            const l_e = (await getLog(theBlock, theBlock, l_evt))!;
            dheadl = [...dheadl, 'log', l_e];
            debug(dheadl);
            const l_ed = (await getEventData(l_evt, l_e))[0];
            dheadl = [...dheadl, 'data', l_ed];
            debug(dheadl);
            const l_edu = l_ctc.unmunge(l_ed);
            dheadl = [...dheadl, 'unmunge', l_edu];
            debug(dheadl);
            return l_edu;
          };
          const getOutput = (o_mode:string, o_lab:string, o_ctc:any): Promise<any> => {
            void(o_mode);
            return _getLog(`oe_${o_lab}`, o_ctc);
          };

          debug(dhead, `OKAY`, ok_vals);
          const theBlockBN = bigNumberify(theBlock);
          const { from } = ok_t;
          const theSecsBN = await getTimeSecs(theBlockBN);
          return {
            data, getOutput, from,
            didTimeout: false,
            time: theBlockBN,
            secs: theSecsBN,
          };
        }
      }
    };

    const viewlib: IViewLib = {
      viewMapRef: async (...args: any): Promise<any> => {
        void(args);
        throw Error('viewMapRef not used by ETH backend'); },
    };
    const views_bin = bin._getViews({reachStdlib}, viewlib);
    const views_namesm = bin._Connectors.ETH.views;
    const getView1 = (vs:BackendViewsInfo, v:string, k:string, vim: BackendViewInfo) =>
      async (...args: any[]): Promise<any> => {
        void(vs);
        const { ty } = vim;
        const ethersC = await getC();
        const vkn = views_namesm[v][k];
        debug('getView1', v, k, 'args', args, vkn, ty);
        try {
          const val = await ethersC[vkn](...args);
          debug('getView1', v, k, 'val', val);
          return ['Some', ty.unmunge(val)];
        } catch (e) {
          debug('getView1', v, k, 'error', e);
          return ['None', null];
        }
    };
    const getViews = getViewsHelper(views_bin, getView1);

    return { getInfo, sendrecv, recv, waitTime: waitUntilTime, waitSecs: waitUntilSecs, iam, selfAddress, getViews, stdlib };
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  };

  async function tokenAccept(token:Token): Promise<void> {
    debug(`tokenAccept: Unnecessary on ETH`, token);
    return;
  };
  const tokenMetadata = async (token: Token): Promise<any> => {
    debug(`tokenMetadata`, token);
    const tokCtc = new ethers.Contract(token, ReachToken_ABI, networkAccount);
    const md: any = {};
    const go = async (t:any, f:string, m:string = f): Promise<void> => {
      debug('tokenMetadata', {f, m});
      try {
        const rv = await tokCtc[m]();
        debug('tokenMetadata', {f, m, rv});
        const v = t ? t.unmunge(rv) : rv;
        debug('tokenMetadata', {f, m, v});
        md[f] = v;
      } catch (e) {
        debug('tokenMetadata', {f, m, e});
      }
    };
    await go(false, 'name');
    await go(false, 'symbol');
    await go(false, 'url');
    await go(false, 'metadata');
    await go(T_UInt, 'supply', 'totalSupply');
    debug(`tokenMetadata`, token, md);
    return md;
  };

  return { deploy, attach, networkAccount, setGasLimit, getGasLimit, setStorageLimit, getStorageLimit, getAddress: selfAddress, stdlib, setDebugLabel, tokenAccept, tokenMetadata };
};

const newAccountFromSecret = async (secret: string): Promise<Account> => {
  const provider = await getProvider();
  const networkAccount = (new ethers.Wallet(secret)).connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
};

const newAccountFromMnemonic = async (phrase: string): Promise<Account> => {
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.fromMnemonic(phrase).connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
};


const getDefaultAccount = async (): Promise<Account> => {
  debug(`getDefaultAccount`);
  if (!canGetDefaultAccount()) throw Error(`Default account not available`);
  return connectAccount(await _getDefaultNetworkAccount());
};

// TODO: Should users be able to access this directly?
// TODO: define a faucet on Ropsten & other testnets?
const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  return await connectAccount(await _getDefaultFaucetNetworkAccount());
});

const createAccount = async () => {
  debug(`createAccount with 0 balance.`);
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.createRandom().connect(provider);
  return await connectAccount(networkAccount);
}

const fundFromFaucet = async (account: AccountTransferable, value: any) => {
  const f = await _specialFundFromFaucet();
  if (f) {
    return await f(account, value);
  } else {
    const faucet = await getFaucet();
    await transfer(faucet, account, value);
  }
};

const newTestAccount = async (startingBalance: any): Promise<Account> => {
  debug('newTestAccount(', startingBalance, ')');
  // requireIsolatedNetwork('newTestAccount'); // XXX is it ok to just let fundFromFaucet err if it can't do it?
  const acc = await createAccount();
  const to = await getAddr(acc);

  try {
    debug('newTestAccount awaiting transfer:', to);
    await fundFromFaucet(acc, startingBalance);
    debug('newTestAccount got transfer:', to);
    return acc;
  } catch (e) {
    console.log(`newTestAccount: Trouble with account ${to}`);
    throw e;
  }
};
const newTestAccounts = make_newTestAccounts(newTestAccount);

const getNetworkTime = async (): Promise<BigNumber> => {
  return bigNumberify(await getNetworkTimeNumber());
};
const getTimeSecs = async (now_bn: BigNumber): Promise<BigNumber> => {
  const now = bigNumberToNumber(now_bn);
  const provider = await getProvider();
  const { timestamp } = await provider.getBlock(now);
  return bigNumberify(timestamp);
};
const getNetworkSecs = async (): Promise<BigNumber> =>
  await getTimeSecs(await getNetworkTime());

const stepTime = async (target: BigNumber): Promise<BigNumber> => {
  void(target);
  if ( isIsolatedNetwork() ) {
    await fundFromFaucet(await getFaucet(), 0);
  } else {
    await Timeout.set(500);
  }
  return await getNetworkTime();
};
const waitUntilTime = make_waitUntilX('time', getNetworkTime, stepTime);

const stepSecs = async (target: BigNumber): Promise<BigNumber> => {
  void(target);
  const now = await stepTime((await getNetworkTime()).add(1));
  return await getTimeSecs(now);
};
const waitUntilSecs = make_waitUntilX('secs', getNetworkSecs, stepSecs);

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  return await waitUntilTime(now.add(delta), onProgress);
};

// Check the contract info and the associated deployed bytecode;
// Verify that:
// * it matches the bytecode you are expecting.
type VerifyResult = {
  creation_block: number,
};
const verifyContract = async (ctcInfo: ContractInfo, backend: Backend): Promise<VerifyResult> => {
  return await verifyContract_(ctcInfo, backend, new EventCache(), 'stdlib');
}
const verifyContract_ = async (ctcInfo: ContractInfo, backend: Backend, eventCache: EventCache, label: string): Promise<VerifyResult> => {
  const { ABI, Bytecode } = backend._Connectors.ETH;
  const address = ctcInfo;
  const iface = new real_ethers.utils.Interface(ABI);
  const dhead = [ 'verifyContract', label ];
  debug(dhead, {address});

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`verifyContract failed: ${msg}`);
    }
  };
  const chkeq = (a: any, e:any, msg:string) => {
    const as = JSON.stringify(a);
    const es = JSON.stringify(e);
    chk(as === es, `${msg}: expected ${es}, got ${as}`);
  };

  const provider = await getProvider();
  const now = await getNetworkTimeNumber();
  const lookupLog = async (event:string): Promise<any> => {
    debug(dhead, 'lookupLog', {event, now});
    while ( eventCache.currentBlock <= now ) {
      const res = await eventCache.queryContract(dhead, address, iface, 0, [ 'time', bigNumberify(now) ], event);
      if ( ! res.succ ) { continue; }
      return res.evt;
    }
    chk(false, `Contract was claimed to be deployed, but the current block is ${now} (cached @ ${eventCache.currentBlock}) and it hasn't been deployed yet.`);
  };
  const e0log = await lookupLog('e0');
  const creation_block = e0log.blockNumber;

  debug(dhead, `checking code...`);
  const dt = await provider.getTransaction( e0log.transactionHash );
  debug(dhead, 'dt', dt);

  const e0p = iface.parseLog(e0log);
  debug(dhead, {e0p});
  const ctorArg = e0p.args;
  debug(dhead, {ctorArg});

  // We don't actually check the live contract code, but instead compare what
  // we would have done to deploy it with how it was actually deployed.
  const actual = dt.data;
  const expected = Bytecode + iface.encodeDeploy(ctorArg).slice(2);
  chkeq(actual, expected, `Contract bytecode does not match expected bytecode.`);

  // We are not checking the balance or the contract storage, because we know
  // that the code is correct and we know that the code mandates the way that
  // those things are initialized

  return { creation_block };
};

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100000000000000000000'
 */
function parseCurrency(amt: CurrencyAmount): BigNumber {
  return bigNumberify(real_ethers.utils.parseUnits(amt.toString(), standardDigits));
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
 * @example  formatCurrency(bigNumberify('100000000000000000000')); // => '100'
 */
function formatCurrency(amt: any, decimals: number = standardDigits): string {
  // Recall that 1 WEI = 10e18 ETH
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Truncate
  decimals = Math.min(decimals, standardDigits);
  const decimalsToForget = standardDigits - decimals;
  const divAmt = bigNumberify(amt)
    .div(bigNumberify(10).pow(decimalsToForget));
  const amtStr = real_ethers.utils.formatUnits(divAmt, decimals);
  // If the str ends with .0, chop it off
  if (amtStr.slice(amtStr.length - 2) == ".0") {
    return amtStr.slice(0, amtStr.length - 2);
  } else {
    return amtStr;
  }
}

/**
 * Formats an account's address in the way users expect to see it.
 * @param acc Account, NetworkAccount, or hex-encoded address
 * @returns the address formatted as a hex-encoded string
 */
function formatAddress(acc: string|NetworkAccount|Account): string {
  return T_Address.canonicalize(acc) as string; // TODO: typing
}

async function launchToken (accCreator:Account, name:string, sym:string) {
  console.log(`Launching token, ${name} (${sym})`);
  const addr = (acc:Account) => acc.networkAccount.address;
  const remoteCtc = ETHstdlib["contracts"]["stdlib.sol:ReachToken"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accCreator.networkAccount);
  console.log(`${sym}: deploy`);
  const supply = bigNumberify(2).pow(256).sub(1);
  const contract = await factory.deploy(name, sym, '', '', supply);
  console.log(`${sym}: wait for deploy: ${contract.deployTransaction.hash}`);
  const deploy_r = await contract.deployTransaction.wait();
  console.log(`${sym}: saw deploy: ${deploy_r.blockNumber}`);
  const id = contract.address;
  console.log(`${sym}: deployed: ${id}`);
  const mint = async (accTo:Account, amt:any) => {
    console.log(`${sym}: transferring ${amt} ${sym} for ${addr(accTo)}`);
    await transfer(accCreator, accTo, amt, id);
  };
  return { name, sym, id, mint };
};

// TODO: restore type ann once types are in place
// const ethLike: EthLike = {
const ethLike = {
  ...ethLikeCompiled,
  ...providerLib,
  getQueryLowerBound,
  setQueryLowerBound,
  getFaucet,
  setFaucet,
  randomUInt,
  hasRandom,
  balanceOf,
  transfer,
  connectAccount,
  newAccountFromSecret,
  newAccountFromMnemonic,
  getDefaultAccount,
  createAccount,
  canFundFromFaucet,
  fundFromFaucet,
  newTestAccount,
  newTestAccounts,
  getNetworkTime,
  waitUntilTime,
  wait,
  getNetworkSecs,
  waitUntilSecs,
  verifyContract,
  standardUnit,
  atomicUnit,
  parseCurrency,
  minimumBalance,
  formatCurrency,
  formatAddress,
  launchToken,
  reachStdlib,
};
return ethLike;
}
