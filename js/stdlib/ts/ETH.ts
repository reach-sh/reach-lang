// ****************************************************************************
// standard library for Javascript users
// ****************************************************************************

import Timeout from 'await-timeout';
import ethers, { Signer } from 'ethers';
import http from 'http';
import url from 'url';
import waitPort from 'wait-port';
import {window, process} from './shim';
import { ConnectorMode, getConnectorMode } from './ConnectorMode';
import {
  add,
  assert,
  bigNumberify,
  debug,
  eq,
  ge,
  getDEBUG,
  isBigNumber,
  lt,
  CurrencyAmount,
  IAccount,
  IContract,
  IRecv,
  OnProgress,
  WPArgs,
  makeRandom,
  argsSplit,
} from './shared';
import {
  memoizeThunk, replaceableThunk
} from './shared_impl';
export * from './shared';
import { AnyETH_Ty, stdlib as compiledStdlib, typeDefs } from './ETH_compiled';


// ****************************************************************************
// Type Definitions
// ****************************************************************************
type BigNumber = ethers.BigNumber;

type Provider = ethers.providers.Provider;
type TransactionReceipt = ethers.providers.TransactionReceipt;
type Wallet = ethers.Wallet;
type Log = ethers.providers.Log;

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

// XXX: when using ganache: nvm use 12
// ganache-core doesn't work with npm version 14 yet
// https://github.com/trufflesuite/ganache-cli/issues/732#issuecomment-623782405

type DeployMode = 'DM_firstMsg' | 'DM_constructor';
type Backend = {_Connectors: {ETH: {
  ABI: string,
  Bytecode: string,
  deployMode: DeployMode
}}};


// TODO: a wrapper obj with smart constructor?
type Address = string;

type NetworkAccount = {
  address?: Address, // required for receivers & deployers
  getAddress?: () => Promise<Address>, // or this for receivers & deployers
  sendTransaction?: (...xs: any) => any, // required for senders
  getBalance?: (...xs: any) => any, // TODO: better type
} | Wallet | Signer; // required to deploy/attach

type ContractInfo = {
  address: Address,
  creation_block: number,
  creator: Address,
  transactionHash?: Hash,
  init?: ContractInitInfo,
};

type Digest = string // XXX
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Digest, Address, AnyETH_Ty>;
type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo>;

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

const connectorMode: ConnectorMode = getConnectorMode();

// Certain functions either behave differently,
// or are only available on an "isolated" network.
// Note: ETH-test-browser-window is NOT considered isolated.
const isIsolatedNetwork: boolean =
  connectorMode.startsWith('ETH-test-dockerized') ||
  connectorMode.startsWith('ETH-test-embedded');

type NetworkDesc =
  {type: 'uri', uri: string, network: string} |
  {type: 'embedded-ganache'} |
  {type: 'window'} |
  {type: 'skip'}

const networkDesc: NetworkDesc =
  connectorMode == 'ETH-test-embedded-ganache' ? {
  type: 'embedded-ganache',
} : (connectorMode == 'ETH-test-dockerized-geth' ||
     connectorMode == 'ETH-live') ? {
  type: 'uri',
  uri: process.env.ETH_NODE_URI || 'http://localhost:8545',
  network: process.env.ETH_NODE_NETWORK || 'unspecified',
} : connectorMode == 'ETH-test-browser-window' ? {
  type: 'window',
} : {
  type: 'skip',
};

const protocolPort = {
  'https:': 443,
  'http:': 80,
};

const getPortConnection = memoizeThunk(async () => {
  debug('getPortConnection');
  if (networkDesc.type != 'uri') { return; }
  const { hostname, port, protocol } = url.parse(networkDesc.uri);
  if (!(protocol === 'http:' || protocol === 'https:')) {
    throw Error(`Unsupported protocol ${protocol}`);
  }
  const args: WPArgs = {
    host: hostname || undefined,
    port: (port && parseInt(port, 10)) || protocolPort[protocol],
    output: 'silent',
    timeout: 1000 * 60 * 1,
  }
  debug('waitPort');
  if (getDEBUG()) {
    console.log(args)
  }
  await waitPort(args);
  debug('waitPort complete');
});

// XXX: doesn't even retry, just returns the first attempt
const doHealthcheck = async (): Promise<void> => {
  debug('doHealthcheck');
  if (networkDesc.type != 'uri') { return; }
  const uriObj = url.parse(networkDesc.uri);

  // XXX the code below only supports http
  if (uriObj.protocol !== 'http:') { return; }

  await new Promise((resolve, reject) => {
    const data = JSON.stringify({
      jsonrpc: '2.0',
      method: 'web3_clientVersion',
      params: [],
      id: 67,
    });
    debug('Sending health check request...');
    const opts = {
      ...uriObj,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': data.length,
      },
    };
    const req = http.request(opts, (res) => {
      debug(`statusCode: ${res.statusCode}`);
      res.on('data', (d) => {
        debug('rpc health check succeeded');
        if (getDEBUG()) {
          process.stdout.write(d);
        }
        resolve({ res, d });
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

const getDevnet = memoizeThunk(async (): Promise<void> => {
  await getPortConnection();
  return await doHealthcheck();
});

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

const rejectInvalidReceiptFor = async (txHash: Hash, r?: TransactionReceipt): Promise<TransactionReceipt> =>
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

const [getProvider, setProvider] = replaceableThunk(async (): Promise<Provider> => {
  if (networkDesc.type == 'uri') {
    await getDevnet();
    const provider = new ethers.providers.JsonRpcProvider(networkDesc.uri);
    provider.pollingInterval = 500; // ms
    return provider;
  } else if (networkDesc.type == 'embedded-ganache') {
    const { default: ganache } = await import('ganache-core');
    if (!ganache) {
      throw Error(`Sorry, optional dependency ganache cannot be found.`)
    }

    const default_balance_ether = 999999999;
    const ganachep = ganache.provider({ default_balance_ether });
    // @ts-ignore
    return new ethers.providers.Web3Provider(ganachep);
  } else if (networkDesc.type == 'window') {
    if (window.ethereum) {
      const provider = new ethers.providers.Web3Provider(window.ethereum);
      // The proper way to ask MetaMask to enable itself is eth_requestAccounts
      // https://eips.ethereum.org/EIPS/eip-1102
      await provider.send('eth_requestAccounts', []);
      return provider;
    } else {
      throw Error(`window.ethereum is not defined`);
    }
  } else {
    // This lib was imported, but not for its net connection.
    throw Error(`Using stdlib/ETH is incompatible with REACH_CONNECTOR_MODE=${connectorMode}`);
  }
});

const getNetworkTimeNumber = async (): Promise<number> => {
  const provider = await getProvider();
  return await provider.getBlockNumber();
};

const fastForwardTo = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  // console.log(`>>> FFWD TO: ${targetTime}`);
  const onProg: OnProgress = onProgress || (() => {});
  requireIsolatedNetwork('fastForwardTo');
  let currentTime;
  while (lt(currentTime = await getNetworkTime(), targetTime)) {
    onProg({ currentTime, targetTime });
    await stepTime();
  }
  // Also report progress at completion time
  onProg({ currentTime, targetTime });
  // console.log(`<<< FFWD TO: ${targetTime} complete. It's ${currentTime}`);
  return currentTime;
};

const requireIsolatedNetwork = (label: string): void => {
  if (!isIsolatedNetwork) {
    throw Error(`Invalid operation ${label} in REACH_CONNECTOR_MODE=${connectorMode}`);
  }
};

const initOrDefaultArgs = (init?: ContractInitInfo): ContractInitInfo2 => ({
  argsMay: init ? Some(init.args) : None,
  value: init ? init.value : bigNumberify(0),
});

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
const actuallyWaitUntilTime = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const onProg: OnProgress = onProgress || (() => {});
  const provider = await getProvider();
  return await new Promise((resolve) => {
    const onBlock = async (currentTimeNum: number | BigNumber) => {
      const currentTime = bigNumberify(currentTimeNum)
      // Does not block on the progress fn if it is async
      onProg({ currentTime, targetTime });
      if (ge(currentTime, targetTime)) {
        provider.off('block', onBlock);
        resolve(currentTime);
      }
    };
    provider.on('block', onBlock);

    // Also "re-emit" the current block
    // Note: this sometimes causes the starting block
    // to be processed twice, which should be harmless.
    getNetworkTime().then(onBlock);
  });
};

const getDummyAccount = memoizeThunk(async (): Promise<Account> => {
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.createRandom().connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
});

const stepTime = async () => {
  requireIsolatedNetwork('stepTime');
  const faucet = await getFaucet();
  const acc = await getDummyAccount();
  return await transfer(faucet, acc, parseCurrency(0));
};

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

export const { addressEq, digest } = compiledStdlib;

export const { T_Null, T_Bool, T_UInt, T_Tuple, T_Array, T_Object, T_Data, T_Bytes, T_Address, T_Digest } = typeDefs;

export const { randomUInt, hasRandom } = makeRandom(32);

export {setProvider};

export const balanceOf = async (acc: Account): Promise<BigNumber> => {
  const { networkAccount } = acc;
  if (!networkAccount) throw Error(`acc.networkAccount missing. Got: ${acc}`);

  if (networkAccount.getBalance) {
    return bigNumberify(await networkAccount.getBalance());
  }

  const addr = await getAddr(acc);
  if (addr) {
    const provider = await getProvider();
    return bigNumberify(await provider.getBalance(addr));
  }
  throw Error(`address missing. Got: ${networkAccount}`);
};

/** @description Arg order follows "src before dst" convention */
export const transfer = async (
  from: AccountTransferable,
  to: AccountTransferable,
  value: BigNumber
): Promise<any> => {
  if (!isBigNumber(value)) throw Error(`Expected a BigNumber: ${value}`);

  const sender = from.networkAccount;
  const receiver = getAddr(to);
  const txn = { to: receiver, value };

  if (!sender || !sender.sendTransaction) throw Error(`Expected from.networkAccount.sendTransaction: ${from}`);
  debug(`sender.sendTransaction(${JSON.stringify(txn)})`);
  return await sender.sendTransaction(txn);
};


export const connectAccount = async (networkAccount: NetworkAccount): Promise<Account> => {
  // @ts-ignore // TODO
  if (networkAccount.getAddress && !networkAccount.address) {
    // @ts-ignore
    networkAccount.address = await getAddr({networkAccount});
  }

  // XXX networkAccount MUST be a Wallet or Signer to deploy/attach
  const provider = await getProvider();
  const address = await getAddr({networkAccount});
  if (!address) { throw Error(`Expected networkAccount.address: ${networkAccount}`); }
  const shad = address.substring(2, 6);

  const iam = (some_addr: Address): Address => {
    if (some_addr == address) {
      return address;
    } else {
      throw Error(`I should be ${some_addr}, but am ${address}`);
    }
  };

  const selfAddress = (): Address => {
    return address;
  }

  const deploy = (bin: Backend): Contract => {
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
      debug(`${shad}: performDeploy with ${JSON.stringify(init)}`);
      const { argsMay, value } = initOrDefaultArgs(init);

      const { ABI, Bytecode } = bin._Connectors.ETH;
      debug(`${shad}: making contract factory`);
      const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);

      (async () => {
        debug(`${shad}: deploying factory`);
        const contract = await factory.deploy(...argsMay, { value });
        debug(`${shad}: deploying factory; done: ${contract.address}`);
        debug(`${shad}: waiting for receipt: ${contract.deployTransaction.hash}`);
        const deploy_r = await contract.deployTransaction.wait();
        debug(`${shad}: got receipt; ${deploy_r.blockNumber}`);
        const info: ContractInfo = {
          address: contract.address,
          creation_block: deploy_r.blockNumber,
          creator: address,
          transactionHash: deploy_r.transactionHash,
          init,
        };
        resolveInfo(info);
      })();
      return attach(bin, infoP);
    };

    const attachDeferDeploy = (): Contract => {
      // impl starts with a shim that deploys on first sendrecv,
      // then replaces itself with the real impl once deployed.
      let impl: Contract = {
        recv: async (...args) => {
          void(args);
          throw Error(`Cannot recv yet; contract is not actually deployed`);
        },
        wait: async (...args) => {
          // Wait times are relative to contract events
          // Wait without an initial contract event is nonsense
          void(args);
          throw Error(`Cannot wait yet; contract is not actually deployed`);
        },
        sendrecv: async (
          label: string, funcNum: number, evt_cnt: number,
          hasLastTime: (BigNumber | false),
          tys: Array<AnyETH_Ty>,
          args: Array<any>, value: BigNumber, out_tys: Array<AnyETH_Ty>,
          onlyIf: boolean, soloSend: boolean,
          timeout_delay: BigNumber | false, sim_p: any,
        ): Promise<Recv> => {
          debug(`${shad}: ${label} sendrecv m${funcNum} (deferred deploy)`);
          void(evt_cnt);
          void(sim_p);
          // TODO: munge/unmunge roundtrip?
          void(hasLastTime);
          void(tys);
          void(out_tys);

          // The following must be true for the first sendrecv.
          try {
            assert(onlyIf, true);
            assert(soloSend, true);
            assert(eq(funcNum, 1));
            assert(!timeout_delay);
          } catch (e) {
            throw Error(`impossible: Deferred deploy sendrecv assumptions violated.\n${e}`);
          }

          // shim impl is replaced with real impl
          impl = performDeploy({args: [[0], args], value});
          await infoP; // Wait for the deploy to actually happen.

          // simulated recv
          return await impl.recv(label, funcNum, evt_cnt, out_tys, false,timeout_delay);
        },
        getInfo: async () => {
          // Danger: deadlock possible
          return await infoP;
        },
        creationTime: (async () => bigNumberify((await infoP).creation_block)),
        // iam/selfAddress don't make sense to check before ctc deploy, but are harmless.
        iam,
        selfAddress,
        stdlib: compiledStdlib,
      };
      // Return a wrapper around the impl. This obj and its fields do not mutate,
      // but the fields are closures around a mutating ref to impl.
      return {
        sendrecv: (...args) => impl.sendrecv(...args),
        recv: (...args) => impl.recv(...args),
        wait: (...args) => impl.wait(...args),
        getInfo: (...args) => impl.getInfo(...args),
        creationTime: (...args) => impl.creationTime(...args),
        iam: (...args) => impl.iam(...args),
        selfAddress: (...args) => impl.selfAddress(...args),
        stdlib: compiledStdlib,
      }
    }

    switch (bin._Connectors.ETH.deployMode) {
      case 'DM_firstMsg':
        return attachDeferDeploy();
      case 'DM_constructor':
        return performDeploy();
      default:
        throw Error(`Unrecognized deployMode: ${bin._Connectors.ETH.deployMode}`);
    };
  };

  const attach = (
    bin: Backend,
    infoP: ContractInfo | Promise<ContractInfo>,
  ): Contract => {
    // unofficially: infoP can also be Contract
    // This should be considered deprecated
    // TODO: remove at next Reach version bump?
    // @ts-ignore
    if (infoP.getInfo) {
      console.log(
        `Calling attach with another Contract is deprecated.`
        + ` Please replace accBob.attach(backend, ctcAlice)`
        + ` with accBob.attach(bin, ctcAlice.getInfo())`
      );
      // @ts-ignore
      infoP = infoP.getInfo();
    }

    const ABI = JSON.parse(bin._Connectors.ETH.ABI);

    // Attached state
    const {getLastBlock, setLastBlock} = (() => {
      let lastBlock: number | null = null;
      const setLastBlock = (n: number): void => {
        debug(`lastBlock from ${lastBlock} to ${n}`);
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

    const updateLast = (o: {blockNumber?: number}): void => {
      if (!o.blockNumber) {
        console.log(o);
        throw Error(`Expected blockNumber in ${Object.keys(o)}`);
      }
      setLastBlock(o.blockNumber);
    };

    const getC = (() => {
      let _ethersC: ethers.Contract | null = null;
      return async () => {
        if (_ethersC) { return _ethersC; }
        const info = await infoP;
        await verifyContract(info, bin);
        debug(`${shad}: contract verified`);
        if (!ethers.Signer.isSigner(networkAccount)) {
          throw Error(`networkAccount must be a Signer (read: Wallet). ${networkAccount}`);
        }
        _ethersC = new ethers.Contract(info.address, ABI, networkAccount);
        return _ethersC;
      }
    })();

    const callC = async (
      funcName: string, arg: any, value: BigNumber,
    ): Promise<{wait: () => Promise<TransactionReceipt>}> => {
      return (await getC())[funcName](arg, { value });
    };

    const getEventData = async (
      ok_evt: string, ok_e: Log
    ): Promise<Array<any>> => {
      const ethersC = await getC();
      const ok_args_abi = ethersC.interface.getEvent(ok_evt).inputs;
      const { args } = ethersC.interface.parseLog(ok_e);
      return ok_args_abi.map(a => args[a.name]);
    };

    const getLogs = async (
      fromBlock: number, toBlock: number, ok_evt: string,
    ): Promise<Array<Log>> => {
      if ( fromBlock > toBlock ) { return []; }
      const ethersC = await getC();
      return await provider.getLogs({
        fromBlock,
        toBlock,
        address: ethersC.address,
        topics: [ethersC.interface.getEventTopic(ok_evt)],
      });
    }

    const getInfo = async () => await infoP;

    const sendrecv_impl = async (
      label: string, funcNum: number, evt_cnt: number,
      hasLastTime: (BigNumber | false), tys: Array<AnyETH_Ty>,
      args: Array<any>, value: BigNumber, out_tys: Array<AnyETH_Ty>,
      onlyIf: boolean, soloSend: boolean,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(hasLastTime);
      const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
        await recv_impl(label, funcNum, out_tys, waitIfNotPresent, timeout_delay);
      if ( ! onlyIf ) {
        return await doRecv(true);
      }

      const funcName = `m${funcNum}`;
      if (tys.length !== args.length) {
        throw Error(`tys.length (${tys.length}) !== args.length (${args.length})`);
      }
      const munged = args.map((m, i) => tys[i].munge(tys[i].canonicalize(m)));
      const [ munged_svs, munged_msg ] = argsSplit(munged, evt_cnt);

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- START --- ${JSON.stringify(munged)}`);
      const lastBlock = await getLastBlock();
      let block_send_attempt = lastBlock;
      let block_repeat_count = 0;
      while (!timeout_delay || lt(block_send_attempt, add(lastBlock, timeout_delay))) {
        let r_maybe: TransactionReceipt | null = null;

        debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY`);
        try {
          const arg = [ munged_svs, munged_msg ];
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- SEND ARG --- ${JSON.stringify(arg)}`);
          const r_fn = await callC(funcName, arg, value);
          r_maybe = await r_fn.wait();

          assert(r_maybe !== null);
          const ok_r = await fetchAndRejectInvalidReceiptFor(r_maybe.transactionHash);

          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- OKAY`);

          // XXX It might be a little dangerous to rely on the polling to just work

          // It may be the case that the next line could speed things up?
          // last_block = ok_r.blockNumber;
          // XXX ^ but do not globally mutate lastBlock.
          // wait relies on lastBlock to refer to the last ctc event
          void(ok_r);

        } catch (e) {

          if ( ! soloSend ) {
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- SKIPPING (${e})`);
          } else {
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- ERROR (${e})`);

          // XXX What should we do...? If we fail, but there's no timeout delay... then we should just die
          await Timeout.set(1);
          const current_block = await getNetworkTimeNumber();
          if (current_block == block_send_attempt) {
            block_repeat_count++;
          }
          block_send_attempt = current_block;
          if ( /* timeout_delay && */ block_repeat_count > 32) {
            if (e.code === 'UNPREDICTABLE_GAS_LIMIT') {
              let error = e;
              while (error.error) { error = error.error; }
              console.log(`impossible: The message you are trying to send appears to be invalid.`);
              console.log(error);
            }
            console.log(`args:`);
            console.log(munged);
            throw Error(`${shad}: ${label} send ${funcName} ${timeout_delay} --- REPEAT @ ${block_send_attempt} x ${block_repeat_count}`);
          }
          debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- TRY FAIL --- ${lastBlock} ${current_block} ${block_repeat_count} ${block_send_attempt}`);
          continue;
          }
        }

        return await doRecv(false);
      }

      // XXX If we were trying to join, but we got sniped, then we'll
      // think that there is a timeout and then we'll wait forever for
      // the timeout message.

      debug(`${shad}: ${label} send ${funcName} ${timeout_delay} --- FAIL/TIMEOUT`);
      return {didTimeout: true};
    };

    const sendrecv = async (
      label: string, funcNum: number, evt_cnt: number, hasLastTime: (BigNumber | false),
      tys: Array<AnyETH_Ty>,
      args: Array<any>, value: BigNumber, out_tys: Array<AnyETH_Ty>,
      onlyIf: boolean, soloSend: boolean,
      timeout_delay: BigNumber | false, sim_p: any,
    ): Promise<Recv> => {
      void(sim_p);
      return await sendrecv_impl(label, funcNum, evt_cnt, hasLastTime, tys, args, value, out_tys, onlyIf, soloSend, timeout_delay);
    }

    // https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
    const recv_impl = async (
      label: string, okNum: number, out_tys: Array<AnyETH_Ty>,
      waitIfNotPresent: boolean,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      const isFirstMsgDeploy = (okNum == 1) && (bin._Connectors.ETH.deployMode == 'DM_firstMsg');
      const lastBlock = await getLastBlock();
      const ok_evt = `e${okNum}`;
      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- START`);

      // look after the last block
      const block_poll_start_init: number =
        lastBlock + (isFirstMsgDeploy ? 0 : 1);
      let block_poll_start: number = block_poll_start_init;
      let block_poll_end = block_poll_start;
      while (!timeout_delay || lt(block_poll_start, add(lastBlock, timeout_delay))) {
        debug(`${shad}: ${label} recv ${ok_evt} --- GET ${block_poll_start} ${block_poll_end}`);
        const es = await getLogs(block_poll_start, block_poll_end, ok_evt);
        if (es.length == 0) {
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- RETRY`);
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
          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY`);

          const ok_e = es[0];
          const ok_r = await fetchAndRejectInvalidReceiptFor(ok_e.transactionHash);
          void(ok_r);
          const ok_t = await provider.getTransaction(ok_e.transactionHash);
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
            console.log(`WARNING: no blockNumber on transaction.`);
            console.log(ok_t);
          }

          debug(`${shad}: ${label} recv ${ok_evt} --- AT ${ok_r.blockNumber}`);
          updateLast(ok_r);
          const ok_ed = await getEventData(ok_evt, ok_e);
          debug(`${shad}: ${label} recv ${ok_evt} --- DATA -- ${JSON.stringify(ok_ed)}`);
          const ok_vals = ok_ed[0][1] || [];
          debug(`${shad}: ${label} recv ${ok_evt} --- MSG -- ${JSON.stringify(ok_vals)}`);
          if (ok_vals.length !== out_tys.length) {
            throw Error(`Expected ${out_tys.length} values from event data, but got ${ok_vals.length}.`);
          }
          const data = ok_vals.map((v: any, i: number) => out_tys[i].unmunge(v));

          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY --- ${JSON.stringify(ok_vals)}`);
          return { didTimeout: false,
                   time: bigNumberify(ok_r.blockNumber),
                   data,
                   value: ok_t.value,
                   from: ok_t.from };
        }
      }

      debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- TIMEOUT`);
      return {didTimeout: true} ;
    };

    const recv = async (
      label: string, okNum: number, ok_cnt: number, out_tys: Array<AnyETH_Ty>,
      waitIfNotPresent: boolean, timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(ok_cnt);
      return await recv_impl(label, okNum, out_tys, waitIfNotPresent, timeout_delay);
    };

    const wait = async (delta: BigNumber) => {
      const lastBlock = await getLastBlock();
      // Don't wait from current time, wait from last_block
      debug(`=====Waiting ${delta} from ${lastBlock}: ${address}`);
      const p = await waitUntilTime(add(lastBlock, delta));
      debug(`=====Done waiting ${delta} from ${lastBlock}: ${address}`);
      return p;
    }

    const creationTime = (async () => bigNumberify((await getInfo()).creation_block));

    // Note: wait is the local one not the global one of the same name.
    return { getInfo, creationTime, sendrecv, recv, wait, iam, selfAddress, stdlib: compiledStdlib };
  };

  return { deploy, attach, networkAccount, stdlib: compiledStdlib };
};

export const newAccountFromSecret = async (secret: string): Promise<Account> => {
  const provider = await getProvider();
  const networkAccount = (new ethers.Wallet(secret)).connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
};

export const newAccountFromMnemonic = async (phrase: string): Promise<Account> => {
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.fromMnemonic(phrase).connect(provider);
  const acc = await connectAccount(networkAccount);
  return acc;
};

export const getDefaultAccount = memoizeThunk(async (): Promise<Account> => {
  debug(`getDefaultAccount`);
  if (isIsolatedNetwork || networkDesc.type == 'window') {
    const provider = await getProvider();
    // TODO: teach ts what the specialized type of provider is in this branch
    // @ts-ignore
    const signer: Signer = provider.getSigner();
    return await connectAccount(signer);
  }
  throw Error(`Default account not available for REACH_CONNECTOR_MODE=${connectorMode}`);
});

// TODO: Should users be able to access this directly?
// TODO: define a faucet on Ropsten & other testnets?
export const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  // XXX this may break if users call setProvider?
  if (isIsolatedNetwork) {
    // On isolated networks, the default account is assumed to be the faucet.
    // Furthermore, it is assumed that the faucet Signer is "unlocked",
    // so no further secrets need be provided in order to access its funds.
    // This is true of reach-provided devnets & embedded ganache.
    // TODO: allow the user to set the faucet via mnemnonic.
    return await getDefaultAccount();
  } else if (networkDesc.type === 'window') {
    // @ts-ignore // 0x539 = 1337
    if (window.ethereum.chainId === '0xNaN' || window.ethereum.chainId == '0x539') {
      // XXX this is a hacky way of checking if we're on a devnet
      // XXX only localhost:8545 is supported
      const p = new ethers.providers.JsonRpcProvider('http://localhost:8545');
      return await connectAccount(p.getSigner());
    }
  }
  throw Error(`getFaucet not supported in this context.`)
});

export const createAccount = async () => {
  debug(`createAccount with 0 balance.`);
  const provider = await getProvider();
  const networkAccount = ethers.Wallet.createRandom().connect(provider);
  return await connectAccount(networkAccount);
}

export const fundFromFaucet = async (account: AccountTransferable, value: BigNumber) => {
  const faucet = await getFaucet();
  await transfer(faucet, account, value);
};

export const newTestAccount = async (startingBalance: BigNumber): Promise<Account> => {
  debug(`newTestAccount(${startingBalance})`);
  requireIsolatedNetwork('newTestAccount');
  const acc = await createAccount();
  const to = getAddr(acc);

  try {
    debug(`newTestAccount awaiting transfer: ${to}`);
    await fundFromFaucet(acc, startingBalance);
    debug(`newTestAccount got transfer: ${to}`);
    return acc;
  } catch (e) {
    console.log(`newTestAccount: Trouble with account ${to}`);
    throw e;
  }
};

export const getNetworkTime = async (): Promise<BigNumber> => {
  return bigNumberify(await getNetworkTimeNumber());
};

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
export const wait = async (delta: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  const now = await getNetworkTime();
  return await waitUntilTime(add(now, delta), onProgress);
};

// onProgress callback is optional, it will be given an obj
// {currentTime, targetTime}
export const waitUntilTime = async (targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> => {
  targetTime = bigNumberify(targetTime);
  if (isIsolatedNetwork) {
    return await fastForwardTo(targetTime, onProgress);
  } else {
    return await actuallyWaitUntilTime(targetTime, onProgress);
  }
};

// Check the contract info and the associated deployed bytecode;
// Verify that:
// * it matches the bytecode you are expecting.
// * it was deployed at exactly creation_block.
// Throws an Error if any verifications fail
export const verifyContract = async (ctcInfo: ContractInfo, backend: Backend): Promise<true> => {
  const { ABI, Bytecode } = backend._Connectors.ETH;
  const { address, creation_block, init, creator } = ctcInfo;
  const { argsMay, value } = initOrDefaultArgs(init);
  const factory = new ethers.ContractFactory(ABI, Bytecode);
  debug(`verifyContract: ${address}`)
  debug(JSON.stringify(ctcInfo));

  const provider = await getProvider();
  const now = await getNetworkTimeNumber();

  const {chainId} = await provider.getNetwork();
  // TODO: allow user to specify lenient verification? (for chains we don't know about)
  const lenient = [
    152709604825713, // https://kovan2.arbitrum.io/rpc
    // XXX ^ this will probably change over time
  ].includes(chainId);
  if (lenient) {
    debug(`verifyContract: using lenient contract verification for chainId=${chainId}`);
  }

  const deployEvent = isNone(argsMay) ? 'e0' : 'e1';
  debug(`verifyContract: checking logs for ${deployEvent}...`);
  // https://docs.ethers.io/v5/api/providers/provider/#Provider-getLogs
  // "Keep in mind that many backends will discard old events"
  // TODO: find another way to validate creation block if much time has passed?
  const logs = await provider.getLogs({
    fromBlock: creation_block,
    toBlock: now,
    address: address,
    topics: [factory.interface.getEventTopic(deployEvent)],
  });
  if (logs.length < 1) {
    throw Error(`Contract was claimed to be deployed at ${creation_block},`
     + ` but the current block is ${now} and it hasn't been deployed yet.`
    );
  }

  const log = logs[0];
  if (log.blockNumber !== creation_block) {
    throw Error(
      `Contract was deployed at blockNumber ${log.blockNumber},`
      + ` but was claimed to be deployed at ${creation_block}.`
    );
  }

  debug(`verifyContract: checking code...`)
  // https://docs.ethers.io/v5/api/providers/provider/#Provider-getCode
  // We can safely getCode at the current block;
  // Reach programs don't change their ETH code over time.
  const actual = await provider.getCode(address);

  // XXX should this also pass {value}, like factory.deploy() does?
  const deployData = factory.getDeployTransaction(...argsMay).data;
  if (typeof deployData !== 'string') {
    // TODO: could also be Ethers.utils.bytes, apparently? Or undefined... why?
    throw Error(`Impossible: deployData is not string ${deployData}`);
  }
  if (!deployData.startsWith(backend._Connectors.ETH.Bytecode)) {
    throw Error(`Impossible: contract with args is not prefixed by backend Bytecode`);
  }

  // FIXME this is based on empirical observation, feels hacky
  // deployData looks like this: [init][setup][body][teardown]
  // actual looks like this:     [init][body]
  // XXX the labels "init", "setup", and "teardown" are probably misleading
  // FIXME: for 0-arg contract deploys, it appears that:
  // * "init" is of length 13
  // * "setup" is not consistent in content, but is of length 156
  // * "teardown" is of length 0
  // FIXME: for n-arg contract deploys, it appears that:
  // * "init" is of length 13
  // * "setup" is of length >= 0 (and probably >= 156)
  // * "teardown" is of length >= 0
  const initLen = 13;
  const setupLen = 156;
  const expected = deployData.slice(0, initLen) + deployData.slice(initLen + setupLen);

  if (expected.length <= 0) {
    throw Error(`Impossible: contract expectation is empty`);
  }

  if (actual !== expected) {
    // FIXME: Empirical observation says that 0-arg contract deploys
    // should === expected. However, this is fragile (?), so it's ok
    // to only pass the next check.

    // FIXME: the 13-char header is also fragile, but we're just
    // running with that assumption for now.

    const deployNoInit = deployData.slice(initLen);
    const actualNoInit = actual.slice(initLen);
    if (actualNoInit.length === 0 || !deployNoInit.includes(actualNoInit)) {
      // FIXME: this display is not so helful for the n-arg contract deploy case.
      const displayLen = 60;
      console.log('--------------------------------------------');
      console.log('expected start: ' + expected.slice(0, displayLen));
      console.log('actual   start: ' + actual.slice(0, displayLen));
      console.log('--------------------------------------------');
      console.log('expected   end: ' + expected.slice(expected.length - displayLen));
      console.log('actual     end: ' + actual.slice(actual.length - displayLen));
      console.log('--------------------------------------------');
      console.log('expected   len: ' + expected.length);
      console.log('actual     len: ' + actual.length);
      console.log('--------------------------------------------');
      throw Error(`Contract bytecode does not match expected bytecode.`);
    }
  }

  // It's tedious to write the next sections w/ lenience, so just skip them.
  // Checking balance & storage at certain old block #s is not supported by some backends.
  if (lenient) return true;

  debug(`verifyContract: checking balance...`);
  const bal = await provider.getBalance(address, creation_block);
  // bal is allowed to exceed expectations, for example,
  // if someone spuriously transferred extra money to the contract
  if (!ge(bal, value)) {
    console.log('bal expected: ' + value);
    console.log('bal actual  : ' + bal);
    throw Error(`Contract initial balance does not match expected initial balance`);
  }

  debug(`verifyContract: checking contract storage...`);
  if (isNone(argsMay)) {
    const st = await provider.getStorageAt(address, 0, creation_block);
    const expectedSt =
      // @ts-ignore XXX
      digest(T_Tuple([T_UInt, T_UInt]),
            [T_UInt.canonicalize(0),
             T_UInt.canonicalize(creation_block)]);
    if (st !== expectedSt) {
      console.log('st expected: ' + expectedSt);
      console.log('st actual  : ' + st);
      throw Error(`Contract initial state does not match expected initial state.`);
    }
  } else {
    // TODO: figure out freeVars using creator and args
    void(creator);
    // const expectedSt = keccak256(1, creation_block, ...freeVars)
    // if st !== expectedSt throw Error
  }

  return true;
};

/** @description the display name of the standard unit of currency for the network */
export const standardUnit = 'ETH';

/** @description the display name of the atomic (smallest) unit of currency for the network */
export const atomicUnit = 'WEI';

/**
 * @description  Parse currency by network
 * @param amt  value in the {@link standardUnit} for the network.
 * @returns  the amount in the {@link atomicUnit} of the network.
 * @example  parseCurrency(100).toString() // => '100000000000000000000'
 */
export function parseCurrency(amt: CurrencyAmount): BigNumber {
  return bigNumberify(ethers.utils.parseUnits(amt.toString(), 'ether'));
}

export const minimumBalance: BigNumber =
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
export function formatCurrency(amt: BigNumber, decimals: number = 18): string {
  // Recall that 1 WEI = 10e18 ETH
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Truncate
  decimals = Math.min(decimals, 18);
  const decimalsToForget = 18 - decimals;
  const divAmt = amt.div(bigNumberify(10).pow(decimalsToForget));
  const amtStr = ethers.utils.formatUnits(divAmt, decimals)
  // If the str ends with .0, chop it off
  if (amtStr.slice(amtStr.length - 2) == ".0") {
    return amtStr.slice(0, amtStr.length - 2);
  } else {
    return amtStr;
  }
}
