import cfxsdk from 'js-conflux-sdk';
import real_ethers from 'ethers';
import * as cfxers from './cfxers';
import { ReachStdlib, IAcc, ICtc, ICtcInfo, Backend, Token } from './classy_shared';
import { CFX_Opts, ETH_Like_Opts } from './classy_opts';
import { CFX_TypeDefs } from './classy_TypeDefs_CFX';
import { ETH_TypeDef } from './classy_TypeDefs_ETH_like';
import { CurrencyAmount, IRecv, OnProgress } from './shared';
import { ConnectorMode, getConnectorMode } from './ConnectorMode';
import Timeout from 'await-timeout';

type BigNumber = real_ethers.BigNumber;
const BigNumber = real_ethers.BigNumber;
type AnyETH_Ty = ETH_TypeDef<unknown, unknown>;
type Address = string
type Recv = IRecv<Address>
type PayAmt = [BigNumber, [BigNumber, Token][]]

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

export const argsSlice = <T>(args: Array<T>, cnt: number): Array<T> =>
  cnt == 0 ? [] : args.slice(-1 * cnt);

export const argsSplit = <T>(args: Array<T>, cnt: number): [ Array<T>, Array<T> ] =>
  cnt == 0 ? [args, []] : [ args.slice(0, args.length - cnt), args.slice(-1 * cnt) ];


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

const initOrDefaultArgs = (init?: ContractInitInfo): ContractInitInfo2 => ({
  argsMay: init ? Some(init.args) : None,
  value: init ? init.value : BigNumber.from(0),
});

export interface IECtcInfo extends ICtcInfo {
  address: string
  creation_block: number
  transactionHash: string
  init?: ContractInitInfo
}

export class Addressed {
  readonly stdlib: ETH_Like<IProvider, INetAcc>; // TODO: CompiledStdlib?
  readonly address: string
  constructor(address: string, stdlib: ETH_Like<IProvider, INetAcc>) {
    this.stdlib = stdlib;
    this.address = address;
  }
  iam(some_addr: string): string {
    // TODO: is it better to use addressEq here?
    // If so, then which one should be returned?
    // const {stdlib, address} = this;
    // if (stdlib.addressEq(some_addr, address)) {
    const {address} = this;
    if (some_addr === address) {
      return some_addr;
    } else {
      throw Error(`I should be ${some_addr}, but am ${address}`);
    }
  }
  /** @deprecated just use acc.address */
  getAddress() {
    return this.address;
  }
}

export class ECtc extends Addressed implements ICtc<AnyETH_Ty> {
  readonly stdlib: ETH_Like<IProvider, INetAcc>
  readonly attacher: EAcc
  private readonly _infoP: Promise<IECtcInfo>|IECtcInfo
  private _lastBlock?: number
  readonly bin: Backend
  private _ethersC?: EthersContract;
  readonly ABI: object

  constructor(attacher: EAcc, bin: Backend, infoP: Promise<IECtcInfo>|IECtcInfo, stdlib: ETH_Like<IProvider, INetAcc>) {
    super(attacher.address, stdlib);
    // TODO: only use the compiled stdlib portion?
    this.attacher = attacher;
    this.bin = bin;
    this.ABI = JSON.parse(bin._Connectors.ETH.ABI);
    this.stdlib = stdlib;
    this._infoP = infoP;
  }
  async getInfo(): Promise<IECtcInfo> {
    return await this._infoP;
  }
  async creationTime(): Promise<BigNumber> {
    return this.stdlib.bigNumberify((await this.getInfo()).creation_block);
  }
  // TODO:
  // The name of this is weird when it's on a ctc,
  // since it's not the ctc's address, it's the attacher's
  selfAddress() { return this.address; }
  async wait(delta: BigNumber): Promise<BigNumber> {
    const {stdlib, address} = this;
    const lastBlock = await this._getLastBlock();
    // Don't wait from current time, wait from last_block
    stdlib.debug('=====Waiting', delta, 'from', lastBlock, ':', address);
    const p = await this._waitUntilTime(stdlib.add(lastBlock, delta));
    stdlib.debug('=====Done waiting', delta, 'from', lastBlock, ':', address);
    return p;
  }
  async _getLastBlock(): Promise<number> {
    if (this._lastBlock === undefined) {
      const info = await this.getInfo();
      this._lastBlock = info.creation_block;
      this.stdlib.debug(`lastBlock initialized to`, this._lastBlock);
    }
    return this._lastBlock;
  }
  setLastBlock(n: number): void {
    this.stdlib.debug(`lastBlock from`, this._lastBlock, `to`, n);
    this._lastBlock = n;
  }
  async _waitUntilTime(targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> {
    const {stdlib} = this;
    targetTime = stdlib.bigNumberify(targetTime);
    if (stdlib.isIsolatedNetwork) {
      return await stdlib.fastForwardTo(targetTime, onProgress);
    } else {
      return await stdlib.actuallyWaitUntilTime(targetTime, onProgress);
    }
  }
  async sendrecv(
    funcNum: number, evt_cnt: number, hasLastTime: (BigNumber | false),
    tys: Array<AnyETH_Ty>,
    args: Array<any>, pay: PayAmt, out_tys: Array<AnyETH_Ty>,
    onlyIf: boolean, soloSend: boolean,
    timeout_delay: BigNumber | false, sim_p: any,
  ): Promise<Recv> {
    void(sim_p);
    return await this._sendrecv_impl(funcNum, evt_cnt, hasLastTime, tys, args, pay, out_tys, onlyIf, soloSend, timeout_delay);
  }
  async recv(
    okNum: number, ok_cnt: number, out_tys: Array<AnyETH_Ty>,
    waitIfNotPresent: boolean, timeout_delay: BigNumber | false,
  ): Promise<Recv> {
    void(ok_cnt);
    return await this._recv_impl(okNum, out_tys, waitIfNotPresent, timeout_delay);
  }
  // private helpers
  private async _sendrecv_impl(
    funcNum: number, evt_cnt: number,
    hasLastTime: (BigNumber | false), tys: Array<AnyETH_Ty>,
    args: Array<any>, pay: PayAmt, out_tys: Array<AnyETH_Ty>,
    onlyIf: boolean, soloSend: boolean,
    timeout_delay: BigNumber | false,
  ): Promise<Recv> {
    const {stdlib} = this;
    const {T_Tuple} = stdlib.typeDefs;
    void(hasLastTime);
    const doRecv = async (waitIfNotPresent: boolean): Promise<Recv> =>
      await this._recv_impl(funcNum, out_tys, waitIfNotPresent, timeout_delay);
    if ( ! onlyIf ) {
      return await doRecv(true);
    }

    const funcName = `m${funcNum}`;
    if (tys.length !== args.length) {
      throw Error(`tys.length (${tys.length}) !== args.length (${args.length})`);
    }

    const dhead = [this.attacher.shad, this.attacher.label, 'send', funcName, timeout_delay, 'SEND'];
    stdlib.debug([...dhead, 'ARGS', args]);
    const [ args_svs, args_msg ] = argsSplit(args, evt_cnt );
    const [ tys_svs, tys_msg ] = argsSplit(tys, evt_cnt);
    // TODO: correct specialized typing on T_Tuple args for ETH_like stdlib
    const arg_ty = T_Tuple([T_Tuple(tys_svs), T_Tuple(tys_msg)]) as unknown as AnyETH_Ty;
    const arg = arg_ty.munge([args_svs, args_msg]);

    stdlib.debug([...dhead, 'START', arg]);
    const lastBlock = await this._getLastBlock();
    let block_send_attempt = lastBlock;
    let block_repeat_count = 0;
    while (!timeout_delay || stdlib.lt(block_send_attempt, stdlib.add(lastBlock, timeout_delay))) {
      stdlib.debug([...dhead, 'TRY']);
      try {
        stdlib.debug([...dhead, 'ARG', arg, pay]);
        await this._callC(dhead, funcName, arg, pay);
      } catch (e) {
        if ( ! soloSend ) {
          stdlib.debug([...dhead, `SKIPPING`, e]);
        } else {
          stdlib.debug([...dhead, `ERROR`, e.stack]);

          // XXX What should we do...? If we fail, but there's no timeout delay... then we should just die
          await Timeout.set(1);
          const current_block = await stdlib.getNetworkTimeNumber();
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
          stdlib.debug([...dhead, `TRY FAIL`, lastBlock, current_block, block_repeat_count, block_send_attempt]);
          continue;
        }
      }

      return await doRecv(false);
    }
    // XXX If we were trying to join, but we got sniped, then we'll
    // think that there is a timeout and then we'll wait forever for
    // the timeout message.

    stdlib.debug([...dhead, `FAIL/TIMEOUT`]);
    return {didTimeout: true};
  }
  private async _recv_impl(
    okNum: number, out_tys: Array<AnyETH_Ty>,
    waitIfNotPresent: boolean,
    timeout_delay: BigNumber | false,
  ): Promise<Recv> {
    // XXX
    void(okNum);
    void(out_tys);
    void(waitIfNotPresent);
    void(timeout_delay);
    throw Error(`TODO _recv_impl`);
  }
  private async _callC(
    dhead: any, funcName: string, arg: any, pay: PayAmt,
  ): Promise<void> {
    const {stdlib, attacher} = this;
    const {ethers} = stdlib;
    const [ value, toks ] = pay;
    const ethersC = await this._getC();
    const zero = stdlib.bigNumberify(0);
    const actualCall = async () =>
      await stdlib.doCall({...dhead, kind:'reach'}, ethersC, funcName, [arg], value, attacher.gasLimit);
    const callTok = async (tok:Token, amt:BigNumber) => {
      const tokCtc = new ethers.Contract(tok, ERC20_ABI, attacher.networkAccount);
      const tokBalance = await tokCtc["balanceOf"](this.address);
      stdlib.debug({...dhead, kind:'token'}, 'balanceOf', tokBalance);
      stdlib.assert(tokBalance >= amt, `local account token balance insufficient: ${tokBalance} < ${amt}`);
      await stdlib.doCall({...dhead, kind:'token'}, tokCtc, "approve", [ethersC.address, amt], zero, attacher.gasLimit); }
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
    }
    await maybePayTok(0);
  }
  private async _getC(): Promise<EthersContract> {
    const {stdlib, attacher} = this;
    const {ethers} = stdlib;
    const {networkAccount} = attacher;
    if (this._ethersC === undefined) {
      const info = await this.getInfo();
      await stdlib.verifyContract(info, this.bin);
      stdlib.debug(attacher.shad, `: contract verified`);
      if (!ethers.Signer.isSigner(attacher.networkAccount)) {
        throw Error(`networkAccount must be a Signer (read: Wallet). ${networkAccount}`);
      }
      const c: EthersContract = new ethers.Contract(info.address, this.ABI, networkAccount);
      this._ethersC = c;
    }
    return this._ethersC;
  }
}

interface EthersContract {
  address: string
}
export class EAcc implements IAcc<AnyETH_Ty> {
  readonly stdlib: ETH_Like<IProvider, INetAcc>
  readonly provider: IProvider
  readonly networkAccount: INetAcc
  readonly address: string
  readonly shad: string
  label: string // writeable, but XXX make it readonly, configurable via constructor
  gasLimit?: BigNumber // writeable, but XXX make it readonly, configurable via constructor
  constructor(networkAccount: INetAcc, address: string, provider: IProvider, stdlib: ETH_Like<IProvider, INetAcc>) {
    this.networkAccount = networkAccount;
    this.address = address;
    this.shad = address.substring(2, 6);
    this.label = this.shad;
    this.provider = provider;
    // TODO: only use the compiled stdlib portion?
    this.stdlib = stdlib;
  }
  iam(some_addr: string): string {
    // TODO: is it better to use addressEq here?
    // If so, then which one should be returned?
    // const {stdlib, address} = this;
    // if (stdlib.addressEq(some_addr, address)) {
    const {address} = this;
    if (some_addr === address) {
      return some_addr;
    } else {
      throw Error(`I should be ${some_addr}, but am ${address}`);
    }
  }
  /** @deprecated just use acc.address */
  getAddress(): string { return this.address; }
  // TODO: deprecate this, just construct it w/ correct label
  setDebugLabel(newLabel: string): this {
    this.label = newLabel;
    return this;
  }
  deploy(bin: Backend): ECtc {
    const {stdlib, networkAccount, shad, gasLimit} = this;
    const {ethers} = stdlib;
    if (!ethers.Signer.isSigner(networkAccount)) {
      throw Error(`Signer required to deploy, ${networkAccount}`);
    }

    const {infoP, resolveInfo} = (() => {
      let resolveInfo = (info: IECtcInfo) => { void(info); };
      const infoP = new Promise<IECtcInfo>(resolve => {
        resolveInfo = resolve;
      });
      return {infoP, resolveInfo};
    })();

    const performDeploy = (
      init?: ContractInitInfo
    ): ECtc => {
      stdlib.debug(shad, ': performDeploy with', init);
      const { argsMay, value } = initOrDefaultArgs(init);

      const { ABI, Bytecode } = bin._Connectors.ETH;
      stdlib.debug(shad, ': making contract factory');
      const factory = new ethers.ContractFactory(ABI, Bytecode, networkAccount);

      (async () => {
        stdlib.debug(shad, `: deploying factory`);
        const contract = await factory.deploy(...argsMay, { value, gasLimit });
        stdlib.debug(shad, `: deploying factory; done:`, contract.address);
        stdlib.debug(shad, `: waiting for receipt:`, contract.deployTransaction.hash);
        const deploy_r = await contract.deployTransaction.wait();
        stdlib.debug(shad, `: got receipt;`, deploy_r.blockNumber);
        const info: IECtcInfo = {
          address: contract.address,
          creation_block: deploy_r.blockNumber,
          transactionHash: deploy_r.transactionHash,
          init,
        };
        resolveInfo(info);
      })();

      return this.attach(bin, infoP);
    }

    return performDeploy();
    // const info = {creation_block: 0, address: 'TODO'};
    // return new ECtc(this, bin, info, this.stdlib);
  }
  attach(bin: Backend, info: Promise<IECtcInfo>|IECtcInfo): ECtc {
    return new ECtc(this, bin, info, this.stdlib);
  }
  // XXX make configurable via constructor
  setGasLimit(ngl: unknown): void {
    this.gasLimit = this.stdlib.bigNumberify(ngl);
  }
}

export interface INetAcc {
  address?: string
  getAddress?(): string|Promise<string>
  sendTransaction(...args: any): Promise<{wait: () => Promise<IReceipt>}>
}

export interface IReceipt {
  transactionHash: string
  status?: unknown
}

export interface IProvider {
  on(evt: string, onBlock: (currentTimeNum: number | BigNumber) => unknown): void
  off(evt: string, onBlock: (currentTimeNum: number | BigNumber) => unknown): void
  getBlockNumber(): Promise<number>
  getTransactionReceipt(txHash: string): Promise<IReceipt>
}

export abstract class ETH_Like<
  Provider extends IProvider,
  NetAcc extends INetAcc,
  > extends ReachStdlib<AnyETH_Ty> {
  private readonly faucetP: Promise<EAcc>
  private readonly _provider: Provider
  private _dummyAccount?: EAcc
  // TODO: figure out how to type ethers
  readonly ethers: any

  readonly isIsolatedNetwork: boolean
  readonly connectorMode: ConnectorMode
  readonly opts: ETH_Like_Opts
  constructor(opts: ETH_Like_Opts = {}) {
    super(opts);
    if (!opts.ethers) throw Error(`impossible: ethers is missing`);
    if (!opts.provider) throw Error(`impossible: provider is missing`);
    this.ethers = opts.ethers;
    this._provider = opts.provider;
    this.opts = super.opts;

    // TODO: not use getConnectorMode() here
    // instead, only inspect opts
    this.connectorMode = getConnectorMode();
    this.isIsolatedNetwork =
      this.connectorMode.startsWith('ETH-test-dockerized')
        || (process.env['REACH_ISOLATED_NETWORK'] ? true : false);

    // TODO: move to ReachStdlib
    if (opts.REACH_FAUCET_SECRET) {
      this.faucetP = this.newAccountFromSecret(opts.REACH_FAUCET_SECRET);
    } else if (opts.REACH_FAUCET_MNEMONIC) {
      this.faucetP = this.newAccountFromMnemonic(opts.REACH_FAUCET_MNEMONIC);
    } else {
      this.faucetP = this._getDefaultFaucet();
    }
  }
  abstract _getDefaultFaucet(): Promise<EAcc>
  abstract verifyContract(ctcInfo: IECtcInfo, backend: Backend): Promise<true>

  prepForDigest(t: ETH_TypeDef<unknown, unknown>, v: unknown): string|[] {
    // Note: abiCoder.encode doesn't correctly handle an empty tuple type
    if (t.paramType === 'tuple()') {
      if (Array.isArray(v) && v.length === 0) {
        return [];
      } else {
        throw Error(`impossible: digest tuple() with non-empty array: ${JSON.stringify(v)}`);
      }
    }
    // XXX use this.ethers in case this needs overridden?
    return real_ethers.utils.defaultAbiCoder.encode([t.paramType], [t.munge(v)])
  }
  tokenEq(a: unknown, b: unknown): boolean {
    const {T_Token} = this.typeDefs;
    return this.bytesEq(T_Token.canonicalize(a), T_Token.canonicalize(b));
  }
  async getProvider(): Promise<Provider> {
    // TODO: wait-port
    return this._provider;
  }
  requireIsolatedNetwork(label: string) {
    if (!this.isIsolatedNetwork) {
      throw Error(`Invalid operation ${label} in REACH_CONNECTOR_MODE=${this.connectorMode}`);
    }
  }

  // Accounts
  async connectAccount(networkAccount: NetAcc): Promise<EAcc> {
    const provider = await this.getProvider();
    const address = networkAccount.address
      || (networkAccount.getAddress && await networkAccount.getAddress());
    if (!address) throw Error(`No address found on networkAccount: ${JSON.stringify(networkAccount)}`);
    return new EAcc(networkAccount, address, provider, this);
  }
  async newAccountFromSecret(secret: string): Promise<EAcc> {
    const provider = await this.getProvider();
    const networkAccount = new this.ethers.Wallet(secret).connect(provider) as NetAcc;
    const acc = await this.connectAccount(networkAccount);
    return acc;
  }
  async newAccountFromMnemonic(mnemonic: string): Promise<EAcc> {
    const provider = await this.getProvider();
    const networkAccount = new this.ethers.Wallet.fromMnemonic(mnemonic).connect(provider) as NetAcc;
    const acc = this.connectAccount(networkAccount);
    return acc;
  }
  async getFaucet(): Promise<EAcc> {
    return await this.faucetP;
  }
  async getDefaultAccount(): Promise<EAcc> {
    throw Error(`TODO: getDefaultAccount`);
  }
  async transfer(from: EAcc, to: EAcc, value: unknown, token?: unknown): Promise<IReceipt> {
    const sender = from.networkAccount;
    const receiver = to.address;
    const valueb = this.bigNumberify(value);

    const dhead = {kind:'transfer'};
    if ( ! token ) {
      const txn = { to: receiver, value: valueb };
      this.debug('sender.sendTransaction(', txn, ')');
      return await this.doTxn(dhead, sender.sendTransaction(txn));
    } else {
      const tokCtc = new this.ethers.Contract(token, ERC20_ABI, sender);
      return await this.doCall(dhead, tokCtc, "transfer", [receiver, valueb], this.bigNumberify(0), undefined);
    }
  }
  async createAccount(): Promise<EAcc> {
    this.debug(`createAccount with 0 balance.`);
    const provider = await this.getProvider();
    const networkAccount = this.ethers.Wallet.createRandom().connect(provider);
    return await this.connectAccount(networkAccount);
  }
  // TODO: move these to ReachStdlib?
  async fundFromFaucet(acc: EAcc, value: unknown, token?: unknown): Promise<unknown> {
    const faucet = await this.getFaucet();
    return await this.transfer(faucet, acc, value, token);
  }
  async newTestAccount(startingBalance: unknown): Promise<EAcc> {
    this.debug('newTestAccount(', startingBalance, ')');
    this.requireIsolatedNetwork('newTestAccount');
    const acc = await this.createAccount();
    const to = acc.address;

    try {
      this.debug('newTestAccount awaiting transfer:', to);
      await this.fundFromFaucet(acc, startingBalance);
      this.debug('newTestAccount got transfer:', to);
      return acc;
    } catch (e) {
      console.log(`newTestAccount: Trouble with account ${to}`);
      throw e;
    }
  }

  // Currency
  formatCurrency(amt: unknown, decimals: number = 18): string {
    // Recall that 1 WEI = 10e18 ETH
    if (!(Number.isInteger(decimals) && 0 <= decimals)) {
      throw Error(`Expected decimals to be a nonnegative integer, but got ${decimals}.`);
    }
    // Truncate
    decimals = Math.min(decimals, 18);
    const decimalsToForget = 18 - decimals;
    const divAmt = this.bigNumberify(amt)
      .div(this.bigNumberify(10).pow(decimalsToForget));
    const amtStr = real_ethers.utils.formatUnits(divAmt, decimals)
    // If the str ends with .0, chop it off
    if (amtStr.slice(amtStr.length - 2) == ".0") {
      return amtStr.slice(0, amtStr.length - 2);
    } else {
      return amtStr;
    }
  }
  parseCurrency(amt: CurrencyAmount): real_ethers.BigNumber {
    return this.bigNumberify(real_ethers.utils.parseUnits(amt.toString(), 'ether'));
  }

  // "public" helper methods
  async getNetworkTime(): Promise<BigNumber> {
    return this.bigNumberify(await this.getNetworkTimeNumber());
  }
  async fastForwardTo(targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> {
    // console.log(`>>> FFWD TO: ${targetTime}`);
    const onProg: OnProgress = onProgress || (() => {});
    this.requireIsolatedNetwork('fastForwardTo');
    let currentTime;
    while (this.lt(currentTime = await this.getNetworkTime(), targetTime)) {
      onProg({ currentTime, targetTime });
      await this.stepTime();
    }
    // Also report progress at completion time
    onProg({ currentTime, targetTime });
    // console.log(`<<< FFWD TO: ${targetTime} complete. It's ${currentTime}`);
    return currentTime;
  }
  async actuallyWaitUntilTime(targetTime: BigNumber, onProgress?: OnProgress): Promise<BigNumber> {
    const onProg: OnProgress = onProgress || (() => {});
    const provider = await this.getProvider();
    return await new Promise((resolve) => {
      const onBlock = async (currentTimeNum: number | BigNumber) => {
        const currentTime = this.bigNumberify(currentTimeNum)
        // Does not block on the progress fn if it is async
        onProg({ currentTime, targetTime });
        if (this.ge(currentTime, targetTime)) {
          provider.off('block', onBlock);
          resolve(currentTime);
        }
      };
      provider.on('block', onBlock);

      // Also "re-emit" the current block
      // Note: this sometimes causes the starting block
      // to be processed twice, which should be harmless.
      this.getNetworkTime().then(onBlock);
    });
  }

  // "private" helper methods
  async getDummyAccount() {
    if (this._dummyAccount === undefined) {
      const provider = await this.getProvider();
      const networkAccount = this.ethers.Wallet.createRandom().connect(provider) as NetAcc;
      const acc = await this.connectAccount(networkAccount);
      this._dummyAccount = acc;
    }
    return this._dummyAccount;
  }
  async stepTime(): Promise<IReceipt> {
    this.requireIsolatedNetwork('stepTime');
    const faucet = await this.getFaucet();
    const acc = await this.getDummyAccount();
    return await this.transfer(faucet, acc, this.parseCurrency(0));
  }
  async getNetworkTimeNumber(): Promise<number> {
    const provider = await this.getProvider();
    return await provider.getBlockNumber();
  }
  private async doTxn(
    dhead: object,
    tp: Promise<{wait: () => Promise<IReceipt>}>,
  ): Promise<IReceipt> {
    this.debug({...dhead, step: `pre call`});
    const rt = await tp;
    this.debug({...dhead, rt, step: `pre wait`});
    const rm = await rt.wait();
    this.debug({...dhead, rt, rm, step: `pre receipt`});
    this.assert(rm !== null, `receipt wait null`);
    const ro = await this.fetchAndRejectInvalidReceiptFor(rm.transactionHash);
    this.debug({...dhead, rt, rm, ro, step: `post receipt`});
    // ro's blockNumber might be interesting
    return ro;
  }

  private async fetchAndRejectInvalidReceiptFor(txHash: string) {
    const provider = await this.getProvider();
    const r = await provider.getTransactionReceipt(txHash);
    return await this.rejectInvalidReceiptFor(txHash, r);
  }

  // TODO: rewrite with async/await
  private async rejectInvalidReceiptFor(txHash: string, r?: IReceipt): Promise<IReceipt> {
    return new Promise((resolve, reject) =>
      !r ? reject(`No receipt for txHash: ${txHash}`) :
      r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`) :
      !r.status ? reject(`Transaction: ${txHash} was reverted by EVM\n${r}`) :
      resolve(r)
    );
  }

  async doCall(
    dhead: object,
    ctc: any, // ethers.Contract
    funcName: string,
    args: Array<any>,
    value: real_ethers.BigNumber,
    gasLimit?: real_ethers.BigNumber,
  ): Promise<IReceipt> {
    const dpre = { ...dhead, funcName, args, value };
    this.debug({...dpre, step: `pre call`});
    return await this.doTxn(
      dpre,
      ctc[funcName](...args, { value, gasLimit },
    ));
  }
}

export function parseNetworkId(opts: CFX_Opts) {
  return opts.networkId
    || (opts.CFX_NETWORK_ID && parseInt(opts.CFX_NETWORK_ID.toString()))
    || CFX.DEFAULT_CFX_NETWORK_ID;
}

export class CFX extends ETH_Like<cfxers.providers.Provider, cfxers.Wallet> {
  static readonly DEFAULT_CFX_NODE_URI = 'http://localhost:12537';
  static readonly DEFAULT_CFX_NETWORK_ID = 999;
  // XXX update this once reachsh/cfx-devnet exists
  static readonly DEFAULT_FAUCET_SECRET = '0x96dc79489f01220ba3f8a7f8a1aaa6741af44e965d21f155a2b2a851e20e1458';
  readonly opts: CFX_Opts;
  readonly standardUnit = 'CFX';
  readonly atomicUnit = 'Drip';
  readonly typeDefs: CFX_TypeDefs;
  readonly connectorMode: ConnectorMode;
  readonly isIsolatedNetwork: boolean;
  constructor(opts: CFX_Opts = {}) {
    super({
      ethers: cfxers,
      provider: new cfxers.providers.Provider(
        new cfxsdk.Conflux({
          url: opts.CFX_NODE_URI || CFX.DEFAULT_CFX_NODE_URI,
          logger: opts.CFX_DEBUG ? console : undefined,
          networkId: parseNetworkId(opts),
        }),
      ),
      ...opts,
    });
    this.opts = {
      networkId: parseNetworkId(opts),
      ...super.opts,
    };
    this.typeDefs = new CFX_TypeDefs(this.opts);
    // XXX support more connector modes
    this.connectorMode = 'CFX-experimental';
    // XXX support non-isolated CFX networks
    this.isIsolatedNetwork = true;
  }
  async _getDefaultFaucet(): Promise<EAcc> {
    return await this.newAccountFromSecret(CFX.DEFAULT_FAUCET_SECRET);
  }
  async verifyContract(...args: any): Promise<true> {
    void(args); // XXX CFX verifyContract
    return true;
  }
}

// const cfx = new CFX();
