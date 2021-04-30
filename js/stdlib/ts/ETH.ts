// ****************************************************************************
// standard library for Javascript users
// ****************************************************************************

import Timeout from 'await-timeout';
import ethers, { Signer } from 'ethers';
import http from 'http';
import url from 'url';
import {window, process} from './shim';
import { ConnectorMode, getConnectorMode } from './ConnectorMode';
import {
  add,
  assert,
  bigNumberify,
  debug, envDefault,
  eq,
  ge,
  getDEBUG,
  lt,
  CurrencyAmount,
  IBackend, IBackendViewInfo, IBackendViewsInfo, getViewsHelper,
  IAccount, IContract, IRecv, deferContract,
  OnProgress,
  makeRandom,
  argsSplit,
} from './shared';
import {
  memoizeThunk, replaceableThunk
} from './shared_impl';
export * from './shared';
import {
  Token,
  PayAmt,
  AnyETH_Ty,
  stdlib as compiledStdlib,
  typeDefs
} from './ETH_compiled';
import waitPort from './waitPort';

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

type DeployMode = 'DM_firstMsg' | 'DM_constructor';
type Backend = IBackend<AnyETH_Ty> & {_Connectors: {ETH: {
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
} | Wallet | Signer; // required to deploy/attach

type ContractInfo = {
  address: Address,
  creation_block: number,
  transactionHash?: Hash,
  init?: ContractInitInfo,
};

type Digest = string
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Digest, Address, Token, AnyETH_Ty>;
type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo>
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

const connectorMode: ConnectorMode = getConnectorMode();

// Certain functions either behave differently,
// or are only available on an "isolated" network.
// Note: ETH-browser is NOT considered isolated.
const isIsolatedNetwork: boolean =
  connectorMode.startsWith('ETH-test-dockerized') ||
  // @ts-ignore
  process.env['REACH_ISOLATED_NETWORK'];

type NetworkDesc =
  {type: 'uri', uri: string, network: string} |
  {type: 'window'} |
  {type: 'skip'}

const networkDesc: NetworkDesc =
  (connectorMode == 'ETH-test-dockerized-geth' ||
   connectorMode == 'ETH-live') ? {
  type: 'uri',
  uri: envDefault(process.env.ETH_NODE_URI, 'http://localhost:8545'),
  network: envDefault(process.env.ETH_NODE_NETWORK, 'unspecified'),
} : connectorMode == 'ETH-browser' ? {
  type: 'window',
} : {
  type: 'skip',
};

const getPortConnection = memoizeThunk(async () => {
  debug('getPortConnection');
  if (networkDesc.type != 'uri') { return; }
  await waitPort(networkDesc.uri);
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
      debug(`statusCode:`, res.statusCode);
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

const stepTime = async (): Promise<TransactionReceipt> => {
  requireIsolatedNetwork('stepTime');
  const faucet = await getFaucet();
  const acc = await getDummyAccount();
  return await transfer(faucet, acc, parseCurrency(0));
};

// ****************************************************************************
// Common Interface Exports
// ****************************************************************************

export const { addressEq, digest } = compiledStdlib;

export const { T_Null, T_Bool, T_UInt, T_Tuple, T_Array, T_Object, T_Data, T_Bytes, T_Address, T_Digest, T_Struct } = typeDefs;

export const { randomUInt, hasRandom } = makeRandom(32);

export {getProvider, setProvider};

// TODO: more providers 'by name'
export type ProviderName
  = 'LocalHost'
  | 'Window'

// TODO
export interface ProviderEnv {
}

// TODO
export function setProviderByEnv(env: ProviderEnv): void {
  void(env);
  throw Error(`setProviderByEnv not yet supported on ETH`);
}

// TODO
export function setProviderByName(providerName: ProviderName): void  {
  void(providerName);
  throw Error(`setProviderByName not yet supported on ETH`);
}

// TODO
export function providerEnvByName(providerName: ProviderName): void {
  void(providerName);
  throw Error(`providerEnvByName not yet supported on ETH`);
}

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
  ctc: ethers.Contract,
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
export const transfer = async (
  from: AccountTransferable,
  to: AccountTransferable,
  value: any,
  token: Token|false = false,
): Promise<any> => {
  const sender = from.networkAccount;
  const receiver = getAddr(to);
  const valueb = bigNumberify(value);

  const dhead = {kind:'transfer'};
  if ( ! token ) {
    const txn = { to: receiver, value: valueb };
    debug('sender.sendTransaction(', txn, ')');
    return await doTxn(dhead, sender.sendTransaction(txn));
  } else {
    const tokCtc = new ethers.Contract(token, ERC20_ABI, sender);
    return await doCall(dhead, tokCtc, "transfer", [receiver, valueb], bigNumberify(0), undefined);
  }
};

const ERC20_ABI = [
  { "constant": false,
    "inputs": [ { "name": "_spender",
                  "type": "address" },
                { "name": "_value",
                  "type": "uint256" } ],
    "name": "approve",
    "outputs": [ { "name": "",
                   "type": "bool" } ],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function" },
  { "constant": true,
    "inputs": [ { "name": "account",
                  "type": "address" } ],
    "name": "balanceOf",
    "outputs": [ { "name": "",
                   "type": "uint256" } ],
    "payable": false,
    "stateMutability": "view",
    "type": "function" },
  { "constant": false,
    "inputs": [ { "name": "_recipient",
                  "type": "address" },
                { "name": "_amount",
                  "type": "uint256" } ],
    "name": "transfer",
    "outputs": [ { "name": "",
                   "type": "bool" } ],
    "payable": false,
    "stateMutability": "nonpayable",
    "type": "function" }
];

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
  let label = shad;

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

  let gasLimit:BigNumber;
  const setGasLimit = (ngl:any): void => {
    gasLimit = bigNumberify(ngl); };

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
        const info: ContractInfo = {
          address: contract.address,
          creation_block: deploy_r.blockNumber,
          transactionHash: deploy_r.transactionHash,
          init,
        };
        resolveInfo(info);
      })();

      return attach(bin, infoP);
    };

    const attachDeferDeploy = (): Contract => {
      let setImpl:any;
      const implP: Promise<Contract> =
        new Promise((resolve) => { setImpl = resolve; });
      const implNow = {
        stdlib: compiledStdlib,
        sendrecv: async (
          funcNum: number, evt_cnt: number,
          hasLastTime: (BigNumber | false),
          tys: Array<AnyETH_Ty>,
          args: Array<any>, pay: PayAmt, out_tys: Array<AnyETH_Ty>,
          onlyIf: boolean, soloSend: boolean,
          timeout_delay: BigNumber | false, sim_p: any,
        ): Promise<Recv> => {
          debug(shad, `:`, label, 'sendrecv m', funcNum, `(deferred deploy)`);
          void(evt_cnt);
          void(sim_p);
          // TODO: munge/unmunge roundtrip?
          void(hasLastTime);
          void(tys);
          void(out_tys);
          const [ value, toks ] = pay;

          // The following must be true for the first sendrecv.
          try {
            assert(onlyIf, `verifyContract: onlyIf must be true`);
            assert(soloSend, `verifyContract: soloSend must be true`);
            assert(eq(funcNum, 1), `verifyContract: funcNum must be 1`);
            assert(!timeout_delay, `verifyContract: no timeout`);
            assert(toks.length == 0, `verifyContract: no tokens`);
          } catch (e) {
            throw Error(`impossible: Deferred deploy sendrecv assumptions violated.\n${e}`);
          }

          // shim impl is replaced with real impl
          setImpl(performDeploy({args: [[0], args], value}));
          await infoP; // Wait for the deploy to actually happen.

          // simulated recv
          return await impl.recv(funcNum, evt_cnt, out_tys, false,timeout_delay);
        },
      };
      const impl: Contract = deferContract(implP, implNow);
      return impl;
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
        debug(`lastBlock from`, lastBlock, `to`, n);
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
      return async (): Promise<ethers.Contract> => {
        if (_ethersC) { return _ethersC; }
        const info = await infoP;
        await verifyContract(info, bin);
        debug(shad, `: contract verified`);
        if (!ethers.Signer.isSigner(networkAccount)) {
          throw Error(`networkAccount must be a Signer (read: Wallet). ${networkAccount}`);
        }
        _ethersC = new ethers.Contract(info.address, ABI, networkAccount);
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
        // @ts-ignore
        const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
        const tokBalance = await tokCtc["balanceOf"](address);
        debug({...dhead, kind:'token'}, 'balanceOf', tokBalance);
        assert(tokBalance >= amt, `local account token balance insufficient: ${tokBalance} < ${amt}`);
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
      funcNum: number, evt_cnt: number,
      hasLastTime: (BigNumber | false), tys: Array<AnyETH_Ty>,
      args: Array<any>, pay: PayAmt, out_tys: Array<AnyETH_Ty>,
      onlyIf: boolean, soloSend: boolean,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(hasLastTime);
      const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
        await recv_impl(funcNum, out_tys, waitIfNotPresent, timeout_delay);
      if ( ! onlyIf ) {
        return await doRecv(true);
      }

      const funcName = `m${funcNum}`;
      if (tys.length !== args.length) {
        throw Error(`tys.length (${tys.length}) !== args.length (${args.length})`);
      }

      const dhead = [shad, label, 'send', funcName, timeout_delay, 'SEND'];
      debug([...dhead, 'ARGS', args]);
      const [ args_svs, args_msg ] = argsSplit(args, evt_cnt );
      const [ tys_svs, tys_msg ] = argsSplit(tys, evt_cnt);
      // @ts-ignore XXX
      const arg_ty = T_Tuple([T_Tuple(tys_svs), T_Tuple(tys_msg)]);
      const arg = arg_ty.munge([args_svs, args_msg]);

      debug([...dhead, 'START', arg]);
      const lastBlock = await getLastBlock();
      let block_send_attempt = lastBlock;
      let block_repeat_count = 0;
      while (!timeout_delay || lt(block_send_attempt, add(lastBlock, timeout_delay))) {
        debug([...dhead, 'TRY']);
        try {
          debug([...dhead, 'ARG', arg, pay]);
          await callC(dhead, funcName, arg, pay);
        } catch (e) {
          if ( ! soloSend ) {
            debug([...dhead, `SKIPPING`, e]);
          } else {
            debug([...dhead, `ERROR`, e.stack]);

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
              console.log(arg);
              throw Error(`${dhead} REPEAT @ ${block_send_attempt} x ${block_repeat_count}`);
            }
            debug([...dhead, `TRY FAIL`, lastBlock, current_block, block_repeat_count, block_send_attempt]);
            continue;
          }
        }

        return await doRecv(false);
      }

      // XXX If we were trying to join, but we got sniped, then we'll
      // think that there is a timeout and then we'll wait forever for
      // the timeout message.

      debug([...dhead, `FAIL/TIMEOUT`]);
      return {didTimeout: true};
    };

    const sendrecv = async (
      funcNum: number, evt_cnt: number, hasLastTime: (BigNumber | false),
      tys: Array<AnyETH_Ty>,
      args: Array<any>, pay: PayAmt, out_tys: Array<AnyETH_Ty>,
      onlyIf: boolean, soloSend: boolean,
      timeout_delay: BigNumber | false, sim_p: any,
    ): Promise<Recv> => {
      void(sim_p);
      return await sendrecv_impl(funcNum, evt_cnt, hasLastTime, tys, args, pay, out_tys, onlyIf, soloSend, timeout_delay);
    }

    // https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
    const recv_impl = async (
      okNum: number, out_tys: Array<AnyETH_Ty>,
      waitIfNotPresent: boolean,
      timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      const isFirstMsgDeploy = (okNum == 1) && (bin._Connectors.ETH.deployMode == 'DM_firstMsg');
      const lastBlock = await getLastBlock();
      const ok_evt = `e${okNum}`;
      debug(shad, ':', label, 'recv', ok_evt, timeout_delay, `--- START`);

      // look after the last block
      const block_poll_start_init: number =
        lastBlock + (isFirstMsgDeploy ? 0 : 1);
      let block_poll_start: number = block_poll_start_init;
      let block_poll_end = block_poll_start;
      while (!timeout_delay || lt(block_poll_start, add(lastBlock, timeout_delay))) {
        debug(shad, ':', label, 'recv', ok_evt, `--- GET`, block_poll_start, block_poll_end);
        const es = await getLogs(block_poll_start, block_poll_end, ok_evt);
        if (es.length == 0) {
          debug(shad, ':', label, 'recv', ok_evt, timeout_delay, `--- RETRY`);
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
          debug(shad, ':', label, 'recv', ok_evt, timeout_delay, `--- OKAY`);

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

          debug(shad, ':', label, 'recv', ok_evt, `--- AT`, ok_r.blockNumber);
          updateLast(ok_r);
          const ok_ed = await getEventData(ok_evt, ok_e);
          debug(shad, ':', label, 'recv', ok_evt, `--- DATA --`, ok_ed);
          const ok_vals = ok_ed[0][1];

          debug(shad, ':', label, 'recv', ok_evt, `--- MSG --`, ok_vals);
          const data = T_Tuple(out_tys).unmunge(ok_vals);

          const getLog = async (l_evt:string, l_ctc:any): Promise<any> => {
            let dhead = [shad, label, 'recv', ok_evt, '--- getLog', l_evt, l_ctc];
            debug(dhead);
            const theBlock = ok_r.blockNumber;
            const l_e = (await getLogs(theBlock, theBlock, l_evt))[0];
            dhead = [...dhead, 'log', l_e];
            debug(dhead);
            const l_ed = (await getEventData(l_evt, l_e))[0];
            dhead = [...dhead, 'data', l_ed];
            debug(dhead);
            const l_edu = l_ctc.unmunge(l_ed);
            dhead = [...dhead, 'unmunge', l_edu];
            debug(dhead);
            return l_edu;
          };
          const getOutput = (o_lab:string, o_ctc:any): Promise<any> =>
            getLog(`oe_${o_lab}`, o_ctc);

          debug(`${shad}: ${label} recv ${ok_evt} ${timeout_delay} --- OKAY --- ${JSON.stringify(ok_vals)}`);
          const { from } = ok_t;
          return {
            data, getOutput, from,
            didTimeout: false,
            time: bigNumberify(ok_r.blockNumber),
          };
        }
      }

      debug(shad, ':', label, 'recv', ok_evt, timeout_delay, '--- TIMEOUT');
      return {didTimeout: true} ;
    };

    const recv = async (
      okNum: number, ok_cnt: number, out_tys: Array<AnyETH_Ty>,
      waitIfNotPresent: boolean, timeout_delay: BigNumber | false,
    ): Promise<Recv> => {
      void(ok_cnt);
      return await recv_impl(okNum, out_tys, waitIfNotPresent, timeout_delay);
    };

    const wait = async (delta: BigNumber) => {
      const lastBlock = await getLastBlock();
      // Don't wait from current time, wait from last_block
      debug('=====Waiting', delta, 'from', lastBlock, ':', address);
      const p = await waitUntilTime(add(lastBlock, delta));
      debug('=====Done waiting', delta, 'from', lastBlock, ':', address);
      return p;
    }

    const creationTime = async () =>
      bigNumberify((await getInfo()).creation_block);

    const views_bin = bin._getViews({reachStdlib: compiledStdlib});
    const views_namesm = bin._Connectors.ETH.views;
    const getView1 = (vs:BackendViewsInfo, v:string, k:string, vim: BackendViewInfo) =>
      async (): Promise<any> => {
        void(vs);
        const { ty } = vim;
        const ethersC = await getC();
        const vkn = views_namesm[v][k];
        const val = await ethersC[vkn]();
        return ty.unmunge(val);
    };
    const getViews = getViewsHelper(views_bin, getView1);

    // Note: wait is the local one not the global one of the same name.
    return { getInfo, creationTime, sendrecv, recv, wait, iam, selfAddress, getViews, stdlib: compiledStdlib };
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  }

  return { deploy, attach, networkAccount, setGasLimit, getAddress: selfAddress, stdlib: compiledStdlib, setDebugLabel };
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

export const getDefaultAccount = async (): Promise<Account> => {
  debug(`getDefaultAccount`);
  if (isIsolatedNetwork || networkDesc.type == 'window') {
    const provider = await getProvider();
    // TODO: teach ts what the specialized type of provider is in this branch
    // @ts-ignore
    const signer: Signer = provider.getSigner();
    return await connectAccount(signer);
  }
  throw Error(`Default account not available for REACH_CONNECTOR_MODE=${connectorMode}`);
};

// TODO: Should users be able to access this directly?
// TODO: define a faucet on Ropsten & other testnets?
export const [getFaucet, setFaucet] = replaceableThunk(async (): Promise<Account> => {
  // XXX this may break if users call setProvider?
  if (isIsolatedNetwork) {
    // On isolated networks, the default account is assumed to be the faucet.
    // Furthermore, it is assumed that the faucet Signer is "unlocked",
    // so no further secrets need be provided in order to access its funds.
    // This is true of reach-provided devnets.
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

export const fundFromFaucet = async (account: AccountTransferable, value: any) => {
  const faucet = await getFaucet();
  await transfer(faucet, account, value);
};

export const newTestAccount = async (startingBalance: any): Promise<Account> => {
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
  const { address, creation_block, init } = ctcInfo;
  const { argsMay } = initOrDefaultArgs(init);
  const factory = new ethers.ContractFactory(ABI, Bytecode);
  debug('verifyContract:', address)
  debug(ctcInfo);

  const provider = await getProvider();
  const now = await getNetworkTimeNumber();

  const deployEvent = isNone(argsMay) ? 'e0' : 'e1';
  debug('verifyContract: checking logs for', deployEvent, '...');
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

  // We are not checking the balance or the contract storage, because we know
  // that the code is correct and we know that the code mandates the way that
  // those things are initialized

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
export function formatCurrency(amt: any, decimals: number = 18): string {
  // Recall that 1 WEI = 10e18 ETH
  if (!(Number.isInteger(decimals) && 0 <= decimals)) {
    throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
  }
  // Truncate
  decimals = Math.min(decimals, 18);
  const decimalsToForget = 18 - decimals;
  const divAmt = bigNumberify(amt)
    .div(bigNumberify(10).pow(decimalsToForget));
  const amtStr = ethers.utils.formatUnits(divAmt, decimals)
  // If the str ends with .0, chop it off
  if (amtStr.slice(amtStr.length - 2) == ".0") {
    return amtStr.slice(0, amtStr.length - 2);
  } else {
    return amtStr;
  }
}

export const reachStdlib = compiledStdlib;
