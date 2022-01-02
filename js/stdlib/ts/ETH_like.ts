import Timeout from 'await-timeout';
import { ethers as real_ethers } from 'ethers';
import {
  assert, protect,
} from './shared_backend';
import type { MaybeRep, MapRefT } from './shared_backend'; // =>
import {
  replaceableThunk,
  debug,
  stdContract, stdVerifyContract,
  stdAccount,
  makeRandom,
  argsSplit,
  ensureConnectorAvailable,
  make_newTestAccounts,
  make_waitUntilX,
  checkTimeout,
  ISetupEventArgs,
  IEventQueue,
  EQGetTxnsR,
  makeEventQueue,
  makeEventStream,
} from './shared_impl';
import {
  bigNumberify,
  bigNumberToNumber,
} from './shared_user';
import ETHstdlib from './stdlib_sol';

// Types-only imports
import type { // =>
  BigNumber,
} from 'ethers';
import type { // =>
  CurrencyAmount,
  IAccount,
  IBackend,
  IViewLib,
  IBackendViewInfo,
  IBackendViewsInfo,
  IContract,
  ISetupArgs, ISetupRes, ISetupViewArgs,
  IRecvArgs, ISendRecvArgs,
  IRecv,
  OnProgress,
} from './shared_impl';
import type { // =>
  AnyETH_Ty,
  Token,
  PayAmt,
} from './ETH_like_compiled';
type ConnectorTy = AnyETH_Ty;
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
import { setQueryLowerBound, getQueryLowerBound } from './shared_impl';
export { setQueryLowerBound, getQueryLowerBound };

// ****************************************************************************
// Type Definitions
// ****************************************************************************

type TransactionReceipt = real_ethers.providers.TransactionReceipt;
type Log = real_ethers.providers.Log;
type Interface = real_ethers.utils.Interface;

// Note: if you want your programs to exit fail
// on unhandled promise rejection, use:
// node --unhandled-rejections=strict

const reachBackendVersion = 7;
const reachEthBackendVersion = 6;
type Backend = IBackend<AnyETH_Ty> & {_Connectors: {ETH: {
  version: number,
  ABI: string,
  Bytecode: string,
  views: {[viewn: string]: string | {[keyn: string]: string}},
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
  _mnemonic?: () => {phrase: string},
} | EthersLikeWallet | EthersLikeSigner; // required to deploy/attach

type ContractInfo = Address;
type SendRecvArgs = ISendRecvArgs<Address, Token, AnyETH_Ty>;
type RecvArgs = IRecvArgs<AnyETH_Ty>;
type Recv = IRecv<Address>
type Contract = IContract<ContractInfo, Address, Token, AnyETH_Ty>;
export type Account = IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>
  & {
    setGasLimit?: (ngl:any) => void
    getGasLimit?: any,
    setStorageLimit?: any,
    getStorageLimit?: any,
  }
type VerifyResult = { creationBlock: BigNumber };
type SetupArgs = ISetupArgs<ContractInfo, VerifyResult>;
type SetupViewArgs = ISetupViewArgs<ContractInfo, VerifyResult>;
type SetupEventArgs = ISetupEventArgs<ContractInfo, VerifyResult>;
type SetupRes = ISetupRes<ContractInfo, Address, Token, AnyETH_Ty>;

type AccountTransferable = Account | {
  networkAccount: NetworkAccount,
  getGasLimit?: any,
  getStorageLimit?: any,
};

// ****************************************************************************
// Helpers
// ****************************************************************************

const reachPublish = (m: string | number) => `_reach_m${m}`
const reachEvent = (e: string | number) => `_reach_e${e}`
const reachOutputEvent = (e: string | number) => `_reach_oe_${e}`;

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
  _specialFundFromFaucet = async () => null,
  canFundFromFaucet,
  standardUnit,
  atomicUnit,
  validQueryWindow: validQueryWindowDefault,
} = ethLikeArgs;

const [getValidQueryWindow, setValidQueryWindow] = replaceableThunk(() => validQueryWindowDefault);

const {
  getProvider
} = providerLib;
const {
  stdlib,
} = ethLikeCompiled;
const {
  T_Address, T_Tuple,
  T_UInt, T_Contract,
  addressEq,
} = stdlib;
const reachStdlib: Stdlib_Backend<AnyETH_Ty> = stdlib;

/** @description convenience function for drilling down to the actual address */
const getAddr = async (acc: AccountTransferable): Promise<Address> => {
  if ( typeof acc === 'string' ) { return acc; }
  if ( ! acc.networkAccount ) throw Error(`Expected acc.networkAccount`);
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

const fetchAndRejectInvalidReceiptFor = async (txHash: Hash): Promise<TransactionReceipt> => {
  const provider = await getProvider();
  const r = await provider.getTransactionReceipt(txHash);
  return await rejectInvalidReceiptFor(txHash, r);
};

const getNetworkTimeNumber = async (): Promise<number> => {
  const provider = await getProvider();
  const ans = await provider.getBlockNumber();
  return ans;
};

const sendRecv_prepArg = (lct:BigNumber, args:Array<any>, tys:Array<any>, evt_cnt:number) => {
  const [ _args_svs, args_msg ] = argsSplit(args, evt_cnt);
  const [ _tys_svs, tys_msg ] = argsSplit(tys, evt_cnt);
  void(_args_svs); void(_tys_svs);
  // @ts-ignore
  const arg_ty = T_Tuple([T_UInt, T_Tuple(tys_msg)]);
  return arg_ty.munge([lct, args_msg]);
};

type EQInitArgs = {
  ctcAddress: Address,
  creationBlock: BigNumber,
};
type EventQueue = IEventQueue<EQInitArgs, TransactionReceipt, TransactionReceipt>;

const bnMax = (x:BigNumber, y:BigNumber): BigNumber =>
  x.lt(y) ? y : x;
const bnMin = (x:BigNumber, y:BigNumber): BigNumber =>
  x.lt(y) ? x : y;

const getTxnTime = (x:{ blockNumber: number }): BigNumber => bigNumberify(x.blockNumber);

const newEventQueue = (): EventQueue => {
  const getTxns = async (lab:string, initArgs:EQInitArgs, ctime: BigNumber, howMany: number): Promise<EQGetTxnsR<TransactionReceipt>> => {
    const dhead = `${lab} getTxns`;
    const { ctcAddress: address, creationBlock } = initArgs;
    const fromBlock = ctime.eq(0) ? creationBlock : ctime.add(1);
    const qw = getValidQueryWindow();
    debug(dhead, { address, fromBlock, qw, howMany });
    if ( howMany > 0 ) { await Timeout.set(1000); }
    let toBlock: BigNumber|undefined = await getNetworkTime();
    if ( qw !== true ) { toBlock = bnMin(toBlock, fromBlock.add(qw)); }
    const toBlock_act = bnMax(fromBlock, toBlock);
    const provider = await getProvider();
    debug(dhead, { toBlock, toBlock_act });
    let logs = [];
    try {
      logs = await provider.getLogs({
        fromBlock: bigNumberToNumber(fromBlock),
        toBlock: bigNumberToNumber(toBlock_act),
        address
      });
    } catch (e) {
      const es = `${e}`;
      debug(dhead, `err`, e, es);
      if ( es.includes('Unable to find block hash') ) {
        debug(dhead, 'ignore');
        toBlock = undefined;
      } else {
        throw e;
      }
    }
    debug(dhead, {logs});
    const txn_hm: {[key: string]: boolean} = {};
    logs.forEach((x:Log) => { txn_hm[x.transactionHash] = true; });
    const txn_hs = Object.keys(txn_hm);
    debug(dhead, {txn_hs});
    const txns: Array<TransactionReceipt> = await Promise.all(txn_hs.map((x:string): Promise<TransactionReceipt> => provider.getTransactionReceipt(x)));
    debug(dhead, {txns});
    return { txns, gtime: toBlock };
  };
  return makeEventQueue<EQInitArgs, TransactionReceipt, TransactionReceipt>({
    raw2proc: ((x) => x),
    alwaysIgnored: (x) => (void(x), false),
    getTxns, getTxnTime,
  });
};

interface LogRep {
  parse: (log: Log) => (any[]|undefined),
  parseA: (txn: TransactionReceipt) => (any[]|undefined),
  parseAb: (txn: TransactionReceipt) => boolean,
};
const makeLogRep = ( getCtcAddress: (() => Address), iface:Interface, evt:string, tys?:AnyETH_Ty[]|undefined): LogRep => {
  debug(`makeLogRep`, { evt, tys });
  const parse = (log:Log): (any[]|undefined) => {
    const { address } = log;
    const ctcAddress = getCtcAddress();
    debug(`parse`, { evt, log, ctcAddress, address });
    if ( ! addressEq(address, ctcAddress) ) { return undefined; }
    const { name, args} = iface.parseLog(log);
    debug(`parse`, {  name, args });
    if ( name !== evt ) { return undefined; }
    if ( tys === undefined ) { return args as any[]; }
    const unargs = tys.map((ty, i) => ty.unmunge(args[i]));
    debug(`parse`, { unargs });
    return unargs;
  };
  const parseA = (txn:TransactionReceipt): (any[]|undefined) => {
    for ( const l of txn.logs ) {
      const p = parse(l);
      debug(`parseA`, { l, p });
      if ( p ) { return p; }
    }
    return undefined;
  };
  const parseAb = (txn:TransactionReceipt) => parseA(txn) !== undefined;
  return { parse, parseA, parseAb };
};
const makeLogRepFor = ( getCtcAddress: (() => Address), iface:Interface, i:number, tys:AnyETH_Ty[]) => {
  debug(`hasLogFor`, i, tys);
  return makeLogRep( getCtcAddress, iface, reachEvent(i), [
    T_Tuple([T_UInt, T_Tuple(tys)])
  ]);
};
const makeHasLogFor = ( getCtcAddress: (() => Address), iface:Interface, i:number, tys:AnyETH_Ty[]) => {
  return makeLogRepFor(getCtcAddress, iface, i, tys).parseAb;
};

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

const ReachToken_ABI = ETHstdlib["contracts"]["sol/stdlib.sol:ReachToken"]["abi"];
const ERC20_ABI = ETHstdlib["contracts"]["sol/stdlib.sol:IERC20"]["abi"];

const balanceOf_token = async (networkAccount: NetworkAccount, address: Address, tok: Token): Promise<BigNumber> => {
  // @ts-ignore
  const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
  return bigNumberify(await tokCtc["balanceOf"](address));
};

const doTxn = async (
  dhead: string,
  tp: Promise<any>,
): Promise<TransactionReceipt> => {
  debug(dhead, { step: `pre call`});
  const rt = await tp;
  debug(dhead, {rt, step: `pre wait`});
  const rm = await rt.wait();
  debug(dhead, {rt, rm, step: `pre receipt`});
  assert(rm !== null, `receipt wait null`);
  const ro = await fetchAndRejectInvalidReceiptFor(rm.transactionHash);
  debug(dhead, {rt, rm, ro, step: `post receipt`});
  return ro;
};

const doCall = async (
  dhead: string,
  ctc: EthersLikeContract,
  funcName: string,
  args: Array<any>,
  value: BigNumber,
  gasLimit: BigNumber|undefined,
  storageLimit: BigNumber|undefined,
): Promise<TransactionReceipt> => {
  const dpre = `${dhead} call ${funcName}`;
  debug(dpre, {args, value, step: `pre call`});
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
): Promise<TransactionReceipt> => {
  const sender = from.networkAccount;
  const receiver = await getAddr(to);
  const valueb = bigNumberify(value);

  const dhead = 'transfer';
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

  const contract = (
    bin: Backend,
    givenInfoP?: Promise<ContractInfo>,
  ): Contract => {
    ensureConnectorAvailable(bin, 'ETH', reachBackendVersion, reachEthBackendVersion);
    const ABI = JSON.parse(bin._Connectors.ETH.ABI);
    const iface = new real_ethers.utils.Interface(ABI);

    const makeGetC = (setupViewArgs:SetupViewArgs, eq: EventQueue) => {
      const { getInfo } = setupViewArgs;
      let _ethersC: EthersLikeContract | null = null;
      return async (): Promise<EthersLikeContract> => {
        if (_ethersC) { return _ethersC; }
        const info = await getInfo();
        const { creationBlock } =
          await stdVerifyContract( setupViewArgs, (async () => {
            return await verifyContract_(info, bin, eq, label);
          }));
        const ctcAddress = info;
        if ( ! eq.isInited() ) {
          eq.init({ ctcAddress, creationBlock });
        }
        debug(label, `contract verified`);
        return (_ethersC = new ethers.Contract(ctcAddress, ABI, networkAccount) as EthersLikeContract);
      };
    };

    const _setup = (setupArgs: SetupArgs): SetupRes => {
      const { setInfo, getInfo, setTrustedVerifyResult } = setupArgs;
      const eq = newEventQueue();

      // Attached state
      const getC = makeGetC(setupArgs, eq);

      const callC = async (
        dhead: any, funcName: string, arg: any, pay: PayAmt,
      ): Promise<TransactionReceipt> => {
        const [ value, toks ] = pay;
        const ethersC = await getC();
        const zero = bigNumberify(0);
        const actualCall = async () =>
          await doCall(`${dhead} callC::reach`, ethersC, funcName, [arg], value, gasLimit, storageLimit);
        const callTok = async (tok:Token, amt:BigNumber) => {
          const tokBalance = await balanceOf_token(networkAccount, address, tok);
          debug({...dhead, kind:'token'}, 'balanceOf', tokBalance);
          assert(tokBalance.gte(amt), `local account token balance is insufficient: ${tokBalance} < ${amt}`);
          // @ts-ignore
          const tokCtc = new ethers.Contract(tok, ERC20_ABI, networkAccount);
          await doCall(`${dhead} callC::token`, tokCtc, "approve", [ethersC.address, amt], zero, gasLimit, storageLimit); }
        const maybePayTok = async (i:number): Promise<TransactionReceipt> => {
          if ( i < toks.length ) {
            const [amt, tok] = toks[i];
            await callTok(tok, amt);
            try {
              return await maybePayTok(i+1);
            } catch (e) {
              await callTok(tok, zero);
              throw e;
            }
          } else {
            return await actualCall();
          }
        };
        return await maybePayTok(0);
      };

      const codec = real_ethers.utils.defaultAbiCoder;
      const decodeEm = (ty:AnyETH_Ty, bs:any): any => {
        const dhead = [label, 'decodeEm'];
        debug(dhead, ty, bs);
        const [ de ] = codec.decode([ty.paramType], bs);
        debug(dhead, de);
        const un = ty.unmunge(de);
        debug(dhead, un);
        return un;
      };
      const getState = async (vibne:BigNumber, tys:Array<AnyETH_Ty>): Promise<Array<any>> => {
        const ethersC = await getC();
        const [ vibna, vsbs ] = await ethersC["_reachCurrentState"]();
        debug(`getState`, { vibne, vibna, vsbs });
        if ( ! vibne.eq(vibna) ) {
          throw Error(`expected state ${vibne}, got ${vibna}`);
        }
        const ty = T_Tuple(tys);
        const res = decodeEm(ty, vsbs);
        debug(`getState`, res);
        // @ts-ignore
        return res;
      };
      const apiMapRef = (i:number, ty:AnyETH_Ty): MapRefT<any> => async (f:string): Promise<MaybeRep<any>> => {
        const dhead = [label, 'apiMapRef'];
        debug(dhead, {i, ty, f});
        const ethersC = await getC();
        const mf = `_reachMap${i}Ref`;
        debug(dhead, mf);
        const mfv = await ethersC[mf](f);
        debug(dhead, { mfv });
        const res = ty.unmunge(mfv);
        debug(dhead, res);
        // @ts-ignore
        return res;
      };

      const canIWin = async (lct:BigNumber): Promise<boolean> => {
        if ( lct.eq(0) ) { return true; }
        const ethersC = await getC();
        let ret = true;
        try {
          const val = await ethersC["_reachCurrentTime"]();
          ret = lct.eq(val);
          debug(label, `canIWin`, {lct, val});
        } catch (e) {
          debug(label, `canIWin`, {e});
        }
        debug(label, `canIWin`, {ret});
        return ret;
      };

      const sendrecv = async (srargs:SendRecvArgs): Promise<Recv> => {
        const { funcNum, evt_cnt, lct, tys, args, pay, out_tys, onlyIf, soloSend, timeoutAt } = srargs;
        const doRecv = async (didSend: boolean, waitIfNotPresent: boolean): Promise<Recv> => {
          if ( ! didSend && lct.eq(0) ) {
            throw new Error(`API call failed`);
          }
          return await recv({funcNum, evt_cnt, out_tys, didSend, waitIfNotPresent, timeoutAt});
        };
        if ( ! onlyIf ) {
          return await doRecv(false, true);
        }

        const funcName = reachPublish(funcNum);
        const dhead = `${label} send ${funcName} ${timeoutAt}`;
        const trustedRecv = async (ok_r:TransactionReceipt): Promise<Recv> => {
          const didSend = true;
          const ethersC = await getC();
          const correctStep = makeHasLogFor((() => ethersC.address), iface, funcNum, out_tys);
          eq.pushIgnore(correctStep);
          return await recvFrom({dhead, out_tys, didSend, funcNum, ok_r});
        };

        debug(dhead, 'ARGS', args);
        const arg = sendRecv_prepArg(lct, args, tys, evt_cnt);
        debug(dhead, 'START', arg);

        if ( funcNum == 0 ) {
          debug(dhead, "deploying");
          const { Bytecode } = bin._Connectors.ETH;
          debug(label, 'making contract factory');
          const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);
          debug(label, `deploying factory`);
          const [ value, toks ] = pay;
          void(toks);
          const overrides = { value, gasLimit };
          if (storageLimit !== undefined) {
            // @ts-ignore
            overrides.storageLimit = storageLimit;
          }
          const contract = await factory.deploy(arg, overrides);
          debug(label, `waiting for receipt:`, contract.deployTransaction.hash);
          const deploy_r = await contract.deployTransaction.wait();
          const ctcAddress: ContractInfo = contract.address;
          const creationBlock = bigNumberify(deploy_r.blockNumber);
          debug(label, `deployed`, { ctcAddress, creationBlock });
          eq.init({ ctcAddress, creationBlock });
          setTrustedVerifyResult({ creationBlock });
          setInfo(ctcAddress);
          return await trustedRecv(deploy_r);
        }

        // Make sure the ctc is available and verified (before we get into try/catch)
        // https://github.com/reach-sh/reach-lang/issues/134
        await getC();

        while ( true ) {
          debug(dhead, 'TIMECHECK', { timeoutAt });
          if ( await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, await getNetworkTimeNumber() + 1) ) {
            debug(dhead, 'FAIL/TIMEOUT');
            return await doRecv(false, false);
          }
          if ( ! soloSend && ! await canIWin(lct) ) {
            debug(dhead, `CANNOT WIN`);
            return await doRecv(false, false);
          }
          let ok_r;
          try {
            debug(dhead, 'ARG', arg, pay);
            ok_r = await callC(dhead, funcName, arg, pay);
          } catch (e:any) {
            debug(dhead, `ERROR`, { stack: e.stack }, e);
            if ( ! soloSend ) {
              debug(dhead, `LOST`);
              return await doRecv(false, false);
            }

            if ( timeoutAt ) {
              // If there can be a timeout, then keep waiting for it
              debug(dhead, `CONTINUE`);
              continue;
            } else {
              // Otherwise, something bad is happening
              throw Error(`${label} failed to call ${funcName}: ${JSON.stringify(e)}`);
            }
          }

          debug(dhead, 'SUCC');
          return await trustedRecv(ok_r);
        }
      };

      type RecvFromArgs = {
        dhead: string,
        out_tys: Array<ConnectorTy>,
        didSend: boolean,
        funcNum: number,
        ok_r: TransactionReceipt,
      };
      const recvFrom = async (rfargs:RecvFromArgs): Promise<Recv> => {
        const { dhead, out_tys, didSend, funcNum, ok_r } = rfargs;
        const theBlock = ok_r.blockNumber;
        debug(dhead, `AT`, theBlock);
        const ethersC = await getC();
        const getCtcAddress = () => ethersC.address;
        const ep = makeLogRepFor(getCtcAddress, iface, funcNum, out_tys).parseA(ok_r);
        if ( ! ep ) { throw Error(`no event log`); }
        const data = ep[0][1];

        debug(dhead, `OKAY`, data);
        const theBlockBN = bigNumberify(theBlock);
        const { from: rawFrom } = ok_r;
        const from = T_Address.canonicalize(rawFrom);
        const theSecsBN = await getTimeSecs(theBlockBN);
        const getOutput = async (o_mode:string, o_lab:string, l_ctc:any, o_val:any): Promise<any> => {
          void(o_mode);
          void(o_val);
          const l_evt = reachOutputEvent(o_lab);
          const lr = makeLogRep(getCtcAddress, iface, l_evt, [ l_ctc ]);
          for ( const l of ok_r.logs ) {
            const r = lr.parse(l);
            debug(dhead, 'getOutput', l_evt, r);
            if ( r ) { return r[0]; }
          }
          throw Error(`no log for ${l_evt}`);
        };
        return {
          data, getOutput, from, didSend,
          didTimeout: false,
          time: theBlockBN,
          secs: theSecsBN,
        };
      };

      // XXX stupidly the same as ALGO.ts's version
      const recv = async (rargs:RecvArgs): Promise<Recv> => {
        const { funcNum, out_tys, didSend, timeoutAt, waitIfNotPresent } = rargs;
        const funcName = `m${funcNum}`;
        const dhead = `${label}: recv ${funcName} ${timeoutAt}`;
        debug(dhead, 'start');
        const ethersC = await getC();
        const didTimeout = async (cr_bn: BigNumber): Promise<boolean> => {
          const cr = bigNumberToNumber(cr_bn);
          debug(dhead, 'TIMECHECK', {timeoutAt, cr_bn, cr});
          const crp = cr + 1;
          const r = await checkTimeout( isIsolatedNetwork, getTimeSecs, timeoutAt, crp);
          debug(dhead, 'TIMECHECK', {r, waitIfNotPresent});
          if ( !r && waitIfNotPresent ) {
            await waitUntilTime(bigNumberify(crp));
          }
          return r;
        };
        const res = await eq.peq(dhead, didTimeout);
        debug(dhead, `res`, res);
        const correctStep = makeHasLogFor((() => ethersC.address), iface, funcNum, out_tys);
        const good = (! res.timeout) && correctStep(res.txn);
        if ( good ) {
          await eq.deq(dhead);
          const txn = res.txn;
          return await recvFrom({dhead, out_tys, didSend, funcNum, ok_r: txn});
        } else if ( timeoutAt ) {
          debug(dhead, `timeout`);
          return { didTimeout: true };
        } else {
          throw Error(`impossible: not good, but no timeout`);
        }
      };

      // Returns address of a Reach contract
      const getContractAddress = getInfo;
      const getContractInfo = getInfo;

      return { getContractInfo, getContractAddress, sendrecv, recv, getState, apiMapRef };
    };

    const setupView = (setupViewArgs: SetupViewArgs) => {
      const eq = newEventQueue();
      const getC = makeGetC(setupViewArgs, eq);
      const viewLib: IViewLib = {
        viewMapRef: async (...args: any): Promise<any> => {
          void(args);
          throw Error('viewMapRef not used by ETH backend'); },
      };
      const views_namesm = bin._Connectors.ETH.views;
      const getView1 = (vs:BackendViewsInfo, v:string, k:string|undefined, vim: BackendViewInfo, isSafe = true) =>
        async (...args: any[]): Promise<any> => {
          void(vs);
          const { ty } = vim;
          const ethersC = await getC();
          const vnv = views_namesm[v];
          const vkn = (typeof vnv === 'string') ? vnv : vnv[k!];
          debug(label, 'getView1', v, k, 'args', args, vkn, ty);
          try {
            const val = await ethersC[vkn](...args);
            debug(label, 'getView1', v, k, 'val', val);
            const uv = ty.unmunge(val);
            return isSafe ? ['Some', uv] : uv;
          } catch (e) {
            debug(label, 'getView1', v, k, 'error', e);
            if (isSafe) {
              return ['None', null];
            } else {
              throw Error(`View ${v}.${k} is not set.`);
            }
          }
        };
      return { getView1, viewLib };
    };

    const setupEvents = (setupArgs: SetupEventArgs) => {
      const createEventStream = (evt: string, tys: AnyETH_Ty[]) => {
        const eq = newEventQueue();
        const getC = makeGetC(setupArgs, eq);
        let ca: Address = '';
        const sync = async () => {
          const c = await getC();
          ca = c.address;
          return;
        };
        const getLogs = (r:TransactionReceipt) => r.logs;
        const lr = makeLogRep( (() => ca), iface, evt, tys);
        const parseLog = lr.parse;
        return makeEventStream<EQInitArgs, TransactionReceipt, TransactionReceipt, Log>({
          eq, getTxnTime, sync, getNetworkTime, getLogs, parseLog,
        });
      };
      return { createEventStream };
    };

    return stdContract({ bin, waitUntilTime, waitUntilSecs, selfAddress, iam, stdlib, setupView, setupEvents, _setup, givenInfoP });
  };

  function setDebugLabel(newLabel: string): Account {
    label = newLabel;
    // @ts-ignore
    return this;
  };

  const tokenAccepted = async (token:Token): Promise<boolean> => {
    debug(`tokenAccepted: Unnecessary on ETHlike`, token);
    return true;
  };
  const tokenAccept = async (token:Token): Promise<void> => {
    debug(`tokenAccept: Unnecessary on ETHlike`, token);
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
    await go(T_UInt, 'decimals');
    debug(`tokenMetadata`, token, md);
    return md;
  };

  return { ...stdAccount({ networkAccount, getAddress: selfAddress, stdlib, setDebugLabel, tokenAccepted, tokenAccept, tokenMetadata, contract }), setGasLimit, getGasLimit, setStorageLimit, getStorageLimit };
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
  const acc = await createAccount();
  const to = await getAddr(acc);

  if (bigNumberify(0).lt(startingBalance)) {
    try {
      debug('newTestAccount awaiting transfer:', to);
      await fundFromFaucet(acc, startingBalance);
      debug('newTestAccount got transfer:', to);
    } catch (e) {
      console.log(`newTestAccount: Trouble with account ${to}`);
      throw e;
    }
  }
  return acc;
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
const verifyContract = async (ctcInfo: ContractInfo, backend: Backend): Promise<VerifyResult> => {
  return await verifyContract_(ctcInfo, backend, newEventQueue(), 'stdlib');
}
const verifyContract_ = async (ctcInfo: ContractInfo, backend: Backend, eq: EventQueue, label: string): Promise<VerifyResult> => {
  const dhead = `${label}: verifyContract`;
  debug(dhead, {ctcInfo});
  const { ABI, Bytecode } = backend._Connectors.ETH;
  const ctcAddress = protect(T_Contract, ctcInfo);
  const iface = new real_ethers.utils.Interface(ABI);
  debug(dhead, {ctcAddress});

  const chk = (p: boolean, msg: string) => {
    if ( !p ) {
      throw Error(`verifyContract failed: ${msg}`);
    }
  };

  // A Reach contract will have a view `_reachCreationTime`
  // where we can see it's creation block. Use this information
  // for querying the contract.
  let creationBlock:BigNumber = bigNumberify(0);
  try {
    const tmpAccount: Account = await createAccount();
    const ctc = new ethers.Contract(ctcAddress, ABI, tmpAccount.networkAccount);
    const creation_time_raw = await ctc["_reachCreationTime"]();
    const creation_time = T_UInt.unmunge(creation_time_raw);
    creationBlock = bigNumberify(creation_time);
  } catch (e) {
    chk(false, `Failed to call the '_reachCreationTime' method on the contract ${ctcAddress} during contract bytecode verification. This could mean that there is a general network fault, or it could mean that the given address is not a Reach contract and does not provide this function. The internal error we caught is: ${e}`);
  }
  eq.init({ ctcAddress, creationBlock });

  const chkeq = (a: any, e:any, msg:string) => {
    const as = JSON.stringify(a);
    const es = JSON.stringify(e);
    chk(as === es, `${msg}: expected ${es}, got ${as}`);
  };

  const r0 = await eq.peq(dhead, (async (bn:BigNumber) => bn.gt(creationBlock)));
  debug(dhead, {r0});
  if ( r0.timeout ) {
    chk(false, `Contract was claimed to be deployed, but the current block is ${r0.time} and it hasn't been deployed yet.`);
    throw Error(`impossible`);
  }
  const e0rec = r0.txn;
  const lr = makeLogRep(() => ctcAddress, iface, reachEvent(0));
  const ctorArg = lr.parseA(e0rec);
  debug(dhead, {e0rec, ctorArg});
  if ( ! ctorArg ) {
    chk(false, `Contract deployment doesn't have first event`);
  }

  // We don't actually check the live contract code, but instead compare what
  // we would have done to deploy it with how it was actually deployed.
  const provider = await getProvider();
  const dt = await provider.getTransaction(e0rec.transactionHash);
  debug(dhead, {dt});
  const actual = dt.data;
  const expected = Bytecode + iface.encodeDeploy(ctorArg).slice(2);
  chkeq(actual, expected, `Contract bytecode does not match expected bytecode.`);

  // We are not checking the balance or the contract storage, because we know
  // that the code is correct and we know that the code mandates the way that
  // those things are initialized

  return { creationBlock };
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

async function launchToken (accCreator:Account, name:string, sym:string, opts:any = {}) {
  debug(`Launching token, ${name} (${sym})`);
  const addr = (acc:Account) => acc.networkAccount.address;
  const remoteCtc = ETHstdlib["contracts"]["sol/stdlib.sol:ReachToken"];
  const remoteABI = remoteCtc["abi"];
  const remoteBytecode = remoteCtc["bin"];
  const factory = new ethers.ContractFactory(remoteABI, remoteBytecode, accCreator.networkAccount);
  debug(`${sym}: deploy`);
  const supply = (opts.supply && bigNumberify(opts.supply)) || bigNumberify(2).pow(256).sub(1);
  const decimals = opts.decimals !== undefined ? opts.decimals : standardDigits;
  const contract = await factory.deploy(name, sym, '', '', supply, decimals);
  debug(`${sym}: wait for deploy: ${contract.deployTransaction.hash}`);
  const deploy_r = await contract.deployTransaction.wait();
  debug(`${sym}: saw deploy: ${deploy_r.blockNumber}`);
  const id = contract.address;
  debug(`${sym}: deployed: ${id}`);
  const mint = async (accTo:Account, amt:any) => {
    debug(`${sym}: transferring ${amt} ${sym} for ${addr(accTo)}`);
    await transfer(accCreator, accTo, amt, id);
  };
  const optOut = async (accFrom:Account, accTo:Account = accCreator) => {
    debug(`${sym}: optOut unnecessary on ETHlike`, accFrom, accTo);
  };
  return { name, sym, id, mint, optOut };
};

function unsafeGetMnemonic(acc: Account|NetworkAccount): string {
  // @ts-ignore
  const networkAccount: NetworkAccount = acc.networkAccount | acc;
  if (networkAccount._mnemonic) {
    return networkAccount._mnemonic().phrase;
  } else {
    throw Error(`unsafeGetMnemonic: Secret key not accessible for account`);
  }
}

// TODO: restore type ann once types are in place
// const ethLike: EthLike = {
const ethLike = {
  ...ethLikeCompiled,
  ...providerLib,
  getQueryLowerBound,
  setQueryLowerBound,
  getValidQueryWindow,
  setValidQueryWindow,
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
  unsafeGetMnemonic,
  launchToken,
  reachStdlib,
};
return ethLike;
}
