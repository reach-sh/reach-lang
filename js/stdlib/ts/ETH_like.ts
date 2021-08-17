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

type DeployMode = 'DM_firstMsg' | 'DM_constructor';
const reachBackendVersion = 1;
const reachEthBackendVersion = 1;
type Backend = IBackend<AnyETH_Ty> & {_Connectors: {ETH: {
  version: number,
  ABI: string,
  Bytecode: string,
  deployMode: DeployMode,
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
  args: Array<any>,
  value: BigNumber,
};

// For either deployment case
type ContractInitInfo2 = {
  argsMay: Maybe<Array<any>>,
  value: BigNumber,
};

type AccountTransferable = Account | {
  networkAccount: NetworkAccount,
}


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
  isWindowProvider,
  _getDefaultNetworkAccount,
  _getDefaultFaucetNetworkAccount,
  _warnTxNoBlockNumber = true,
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
  return await provider.getBlockNumber();
};

const requireIsolatedNetwork = (label: string): void => {
  if (!isIsolatedNetwork()) {
    throw Error(`Invalid operation ${label}; network is not isolated`);
  }
};

const initOrDefaultArgs = (init?: ContractInitInfo): ContractInitInfo2 => ({
  argsMay: init ? Some(init.args) : None,
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


class EventCache {

  cache: any[] = [];

  public currentBlock = 0;

  constructor() {
    this.cache = [];
  }

  async query_(fromBlock: number, toBlock: number, topic: string, getLogs: (currentBlock: number) => Promise<[any[], number]>) {
    debug(`EventCache.query`, fromBlock, toBlock, topic);
    if (fromBlock > toBlock) {
      return undefined;
    }
    // Clear cache of stale transactions
    // Cache's min bound will be `fromBlock`
    this.cache = this.cache.filter((x) => x.blockNumber >= fromBlock);

    // When checking for topic, only choose blocks that are lte `toBlock`
    const filterFn = (x: any) =>
      x.topics.includes(topic.toString())
      && x.blockNumber <= toBlock;

    // Check to see if the transaction we want is in the cache
    const initLogs = this.cache.filter(filterFn);
    if(initLogs.length > 0) {
      debug(`Found transaction in Event Cache`);
      return getMinBlock(initLogs);
    }

    debug(`Transaction not in Event Cache. Querying network...`);

    // If no results, then contact network
    const currentTime = await getNetworkTimeNumber();
    const [ res, toBlock_eff ] = await getLogs(this.currentBlock + 1);
    this.cache = res;

    this.currentBlock =
      (this.cache.length == 0)
        ? Math.min(currentTime, toBlock_eff)
        : getMaxBlock(this.cache).blockNumber;

    // Check for pred again
    const foundLogs = this.cache.filter(filterFn);

    if (foundLogs.length < 1) {
      return undefined;
    }

    return getMinBlock(foundLogs);
  }

  async doGetLogs(fromBlock: number, toBlock_given: number, address: string): Promise<[any[], number]> {
    const provider = await getProvider();
    const toBlock =
      validQueryWindow === true
      ? toBlock_given
      : Math.min(toBlock_given, fromBlock + validQueryWindow);
    debug(`doGetLogs`, { fromBlock, toBlock });
    const res = await provider.getLogs({
      fromBlock,
      toBlock,
      address,
    });
    return [ res, toBlock ];
  };

  async queryContract(fromBlock: number, toBlock: number, address: string, event: string, iface: any) {
    const topic = iface.getEventTopic(event);
    const getLogs = async (currentBlock: number) => {
      return await this.doGetLogs(
        Math.max(currentBlock, fromBlock),
        toBlock,
        address,
      );
    };
    return await this.query_(fromBlock, toBlock, topic, getLogs);
  }

  async query(fromBlock: number, toBlock: number, ok_evt: string, getC: () => Promise<EthersLikeContract>) {
    const ethersC = await getC();
    const topic = ethersC.interface.getEventTopic(ok_evt);
    const getLogs = async (currentBlock: number) => {
      return await this.doGetLogs(
        (currentBlock > fromBlock && currentBlock < toBlock) ? currentBlock : fromBlock,
        toBlock,
        ethersC.address,
      );
    };
    return await this.query_(fromBlock, toBlock, topic, getLogs);
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
): Promise<void> => {
  const dpre = { ...dhead, funcName, args, value };
  debug({...dpre, step: `pre call`});
  return await doTxn(
    dpre,
    ctc[funcName](...args, { value, gasLimit }));
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
    // @ts-ignore
    const gl = from.getGasLimit ? from.getGasLimit() : undefined;
    return await doCall(dhead, tokCtc, "transfer", [receiver, valueb], bigNumberify(0), gl);
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

  let gasLimit:BigNumber;
  const setGasLimit = (ngl:any): void => {
    gasLimit = bigNumberify(ngl); };
  const getGasLimit = (): BigNumber => gasLimit;

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
      const { argsMay, value } = initOrDefaultArgs(init);

      const { ABI, Bytecode } = bin._Connectors.ETH;
      debug(shad, ': making contract factory');
      const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);

      (async () => {
        debug(shad, `: deploying factory`);
        const contract = await factory.deploy(...argsMay, { value, gasLimit });
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
          const { funcNum, evt_cnt, out_tys, args, pay, onlyIf, soloSend, timeoutAt } = srargs;
          debug(shad, `:`, label, 'sendrecv m', funcNum, `(deferred deploy)`);
          const [ value, toks ] = pay;

          // The following must be true for the first sendrecv.
          try {
            assert(onlyIf, `verifyContract: onlyIf must be true`);
            assert(soloSend, `verifyContract: soloSend must be true`);
            assert(eq(funcNum, 1), `verifyContract: funcNum must be 1`);
            assert(!timeoutAt, `verifyContract: no timeout`);
            assert(toks.length == 0, `verifyContract: no tokens`);
          } catch (e) {
            throw Error(`impossible: Deferred deploy sendrecv assumptions violated.\n${e}`);
          }

          // shim impl is replaced with real impl
          setImpl(performDeploy({args: [[0], args], value}));
          await infoP; // Wait for the deploy to actually happen.

          // simulated recv
          return await impl.recv({funcNum, evt_cnt, out_tys, waitIfNotPresent: false, timeoutAt});
        },
      };
      const impl: Contract = deferContract(true, implP, implNow);
      return impl;
    }

    const { deployMode } = bin._Connectors.ETH;
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

    let theCreationTime: number|undefined = undefined;
    const getC = (() => {
      let _ethersC: EthersLikeContract | null = null;
      return async (): Promise<EthersLikeContract> => {
        if (_ethersC) { return _ethersC; }
        const info = await infoP;
        const { creation_block } = await verifyContract_(info, bin, eventCache);
        theCreationTime = creation_block;
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
        await doCall({...dhead, kind:'reach'}, ethersC, funcName, [arg], value, gasLimit);
      const callTok = async (tok:Token, amt:BigNumber) => {
        const tokBalance = await balanceOf_token(networkAccount, address, tok);
        debug({...dhead, kind:'token'}, 'balanceOf', tokBalance);
        assert(tokBalance.gte(amt), `local account token balance is insufficient: ${tokBalance} < ${amt}`);
        // @ts-ignore
        const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
        await doCall({...dhead, kind:'token'}, tokCtc, "approve", [ethersC.address, amt], zero, gasLimit); }
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
      return await eventCache.query(fromBlock, toBlock, ok_evt, getC);
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
      const [ args_svs, args_msg ] = argsSplit(args, evt_cnt );
      const [ tys_svs, tys_msg ] = argsSplit(tys, evt_cnt);
      // @ts-ignore XXX
      const arg_ty = T_Tuple([T_Tuple(tys_svs), T_Tuple(tys_msg)]);
      const arg = arg_ty.munge([args_svs, args_msg]);

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
            debug(...dhead, `ERROR`, e.stack);

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
      const isFirstMsgDeploy = (funcNum == 1) && (bin._Connectors.ETH.deployMode == 'DM_firstMsg');
      const lastBlock = await getLastBlock();
      const ok_evt = `e${funcNum}`;
      const dhead = { t: 'recv', label, ok_evt };
      debug(dhead, `START`);

      // look after the last block
      const block_poll_start_init: number =
        lastBlock + (isFirstMsgDeploy ? 0 : 1);
      let block_poll_start: number = block_poll_start_init;
      let block_poll_end = block_poll_start;
      while ( ! await checkTimeout(getTimeSecs, timeoutAt, block_poll_start) ) {
        debug(dhead, `GET`, { block_poll_start, block_poll_end });
        const ok_e = await eventCache.query(block_poll_start, block_poll_end, ok_evt, getC);
        if (ok_e == undefined) {
          debug(dhead, `RETRY`);
          block_poll_start = block_poll_end;

          await Timeout.set(1);
          block_poll_end = await getNetworkTimeNumber();
          if ( waitIfNotPresent && block_poll_start == block_poll_end ) {
            await waitUntilTime(bigNumberify(block_poll_end + 1));
          }
          if ( block_poll_start <= lastBlock ) {
            block_poll_start = block_poll_start_init; }

          continue;
        } else {
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

      debug(shad, ':', label, 'recv', ok_evt, timeoutAt, '--- TIMEOUT');
      return {didTimeout: true} ;
    };

    const creationTime = async () => {
      await getC();
      // @ts-ignore
      return bigNumberify(theCreationTime);
    };
    const creationSecs = async () =>
      await getTimeSecs(await creationTime());

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

    return { getInfo, creationTime, creationSecs, sendrecv, recv, waitTime: waitUntilTime, waitSecs: waitUntilSecs, iam, selfAddress, getViews, stdlib };
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

  return { deploy, attach, networkAccount, setGasLimit, getGasLimit, getAddress: selfAddress, stdlib, setDebugLabel, tokenAccept, tokenMetadata };
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
  if (!(isWindowProvider() || isIsolatedNetwork())) throw Error(`Default account not available`);
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
  const faucet = await getFaucet();
  await transfer(faucet, account, value);
};

const newTestAccount = async (startingBalance: any): Promise<Account> => {
  debug('newTestAccount(', startingBalance, ')');
  requireIsolatedNetwork('newTestAccount');
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
  return await verifyContract_(ctcInfo, backend, new EventCache());
}
const verifyContract_ = async (ctcInfo: ContractInfo, backend: Backend, eventCache: EventCache): Promise<VerifyResult> => {
  const { ABI, Bytecode, deployMode } = backend._Connectors.ETH;
  const address = ctcInfo;
  const iface = new real_ethers.utils.Interface(ABI);
  debug('verifyContract', {address});

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
  const getLogs = async (event:string): Promise<any> => {
    debug('verifyContract: getLogs', {event, now});
    while ( eventCache.currentBlock <= now ) {
      const log = await eventCache.queryContract(0, now, address, event, iface);
      if ( log === undefined ) { continue; }
      return log;
    }
    chk(false, `Contract was claimed to be deployed, but the current block is ${now} (cached @ ${eventCache.currentBlock}) and it hasn't been deployed yet.`);
  };
  const e0log = await getLogs('e0');
  const creation_block = e0log.blockNumber;

  debug(`verifyContract: checking code...`);
  const dt = await provider.getTransaction( e0log.transactionHash );
  debug('dt', dt);

  const ctorArgs = await (async (): Promise<any> => {
    switch ( deployMode ) {
      case 'DM_firstMsg': {
        const e1log = await getLogs('e1');
        const e1p = iface.parseLog(e1log);
        debug(`e1p`, e1p);
        return e1p.args;
      }
      case 'DM_constructor':
        return [];
      default:
        throw Error(`Unrecognized deployMode: ${deployMode}`);
    }
  })();

  // We don't actually check the live contract code, but instead compare what
  // we would have done to deploy it with how it was actually deployed.
  const actual = dt.data;
  const expected = Bytecode + iface.encodeDeploy(ctorArgs).slice(2);
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

// TODO: restore type ann once types are in place
// const ethLike: EthLike = {
const ethLike = {
  ...ethLikeCompiled,
  ...providerLib,
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
  reachStdlib,
};
return ethLike;
}
