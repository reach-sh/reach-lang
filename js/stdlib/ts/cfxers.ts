import cfxsdk from 'js-conflux-sdk';
import { ethers } from 'ethers';
import * as providers from './cfxers_providers';
import { ParamType } from '@ethersproject/abi';
const { BigNumber, utils } = ethers;
export { BigNumber, utils, providers }
import { address_cfxStandardize } from './CFX_util';
import Timeout from 'await-timeout';
import { debug } from './shared_impl';

// XXX Convenience export, may want to rethink
export { cfxsdk };

// This file immitates the ethers.js API

// Recursively stringify BigNumbers
function unbn(arg: any): any {
  if (!arg) return arg;
  if (arg._isBigNumber) return arg.toString();
  if (Array.isArray(arg)) return arg.map(unbn);
  if (typeof arg === 'string') return arg;
  if (Object.keys(arg).length > 0) {
    const newArg: {[k: string]: any} = {};
    for (const k of Object.keys(arg)) {
      newArg[k] = unbn(arg[k]);
    }
    return newArg;
  }
  return arg;
}

function booleanize(arg: any): boolean {
  if (typeof arg === 'boolean') return arg;
  if (typeof arg === 'number') return arg !== 0;

  // I don't quite understand why bools get represented this way sometimes, but they do.
  if (Array.isArray(arg) && arg.length === 1) return booleanize(arg[0]);

  // XXX handle more stuff
  throw Error(`don't know how to booleanize '${arg}': ${typeof arg}`);
}

function conform(args: any[], tys: ParamType[]): any[] {
  // XXX find a better way to do this stuff.
  args = unbn(args);
  if (Array.isArray(args)) {
    if (args.length !== tys.length) {
      debug(`conform`, `err`, {args, tys});
      throw Error(`impossible: number of args (${args.length}) does not match number of tys (${tys.length})`);
    }
    for (const i in tys) {
      if (tys[i].type === 'tuple') {
        args[i] = conform(args[i], tys[i].components);
      } else if (tys[i].type === 'bool') {
        args[i] = booleanize(args[i]);
      } else {
        // XXX handle more stuff
        // debug(`conform untouched:`, args[i], tys[i])
      }
    }
  }
  return args;
}

export class Signer {
  static isSigner(x: any) {
    // XXX
    return x instanceof Wallet;
  }
}

interface IContract {
  [key: string]: any
}

// compare to ethers.Contract
export class Contract implements IContract {
  [k: string]: any
  _abi: any[]
  _wallet: Wallet
  _receiptP?: Promise<any>
  _contract: cfxsdk.Contract
  address?: string
  deployTransaction: {
    hash?: string,
    wait: () => Promise<{
      blockNumber: number,
      transactionHash: string,
    }>,
  }

  // const ok_args_abi = ethersC.interface.getEvent(ok_evt).inputs;
  // const { args } = ethersC.interface.parseLog(ok_e);
  // return ok_args_abi.map((a: any) => args[a.name]);
  interface: ethers.utils.Interface
  // {
  //   getEventTopic: (name: string) => string, // ?
  //   getEvent: (name: string) => {inputs: {name: string}[]},
  //   parseLog: (log: Log) => {args: {[k: string]: any}},
  // }

  constructor(address: string|null|undefined, abi: string|any[], wallet: Wallet, receiptP?: Promise<any>, hash?: string) {
    this.address = address || undefined;
    this._abi = (typeof abi === 'string') ? JSON.parse(abi) : abi;
    this._wallet = wallet;
    this._receiptP = receiptP;
    // @ts-ignore // ???
    this._contract = this._wallet.provider.conflux.Contract({
      abi: this._abi, address: this.address,
    });
    const self = this;
    this.deployTransaction = {
      hash,
      wait: async () => {
        debug(`cfxers:Contract.wait`, `start`);
        if (!receiptP) {
          throw Error(`No receipt promise to wait on`);
        }
        const receipt = await self._receiptP;
        debug(`cfxers:Contract.wait`, `got receipt`, receipt);
        if (self.address && self.address !== receipt.contractCreated) {
          throw Error(`Impossible: ctc addresses don't match: ${self.address} vs ${receipt.contractCreated}`);
        }
        self.address = self.address || receipt.contractCreated;
        if (self.deployTransaction.hash && self.deployTransaction.hash !== receipt.transactionHash) {
          throw Error(`Impossible: txn hashes don't match: ${self.deployTransaction.hash} vs ${receipt.transactionHash}`);
        }
        self.deployTransaction.hash = self.deployTransaction.hash || receipt.transactionHash;
        return providers.ethifyOkReceipt(receipt);
      },
    }
    this.interface = new ethers.utils.Interface(this._abi);
    for (const item of this._abi) {
      if (item.type === 'function') {
        if (item.name[0] !== '_' && item.name !== 'address' && item.name !== 'deployTransaction' && item.name !== 'interface') {
          this[item.name] = this._makeHandler(item);
        }
      }
    }
  }

  _makeHandler(abiFn: any): any {
    const iface = this.interface;
    const fname: string = abiFn.name;
    const from = this._wallet.getAddress();
    const self = this;
    // XXX this should always be safe but maybe error handling around it just in case?
    // XXX handle the case where the same method name can have multiple input sizes/types?
    const inputs = iface.fragments.filter((x) => x.name == fname)[0].inputs;
    return async (...args: any) => {
      debug(`cfxers:handler`, fname, 'call', {args});
      let txn: {from?: string, value?: string}|null = null;
      if (args.length === inputs.length + 1) {
        txn = unbn(args.pop());
        txn = txn && {from, ...txn, value: (txn.value || '0').toString()};
      }
      args = unbn(args);
      const argsConformed = conform(args, inputs);
      debug(`cfxers:handler`, fname, 'conform', argsConformed);

      // XXX using presence of txn to decide this is sketchy
      // should instead figure out from ABI if this is a view-only fn?
      if (txn) {
        // Note: this usage of `.call` here is because javascript is insane.
        // XXX 2021-06-14 Dan: This works for the cjs compilation target, but does it work for the other targets?
        // @ts-ignore
        const transactionReceipt = await self._contract[fname].call(...argsConformed).sendTransaction(txn).executed();
        debug(`cfxers:handler`, fname, 'receipt');
        debug(transactionReceipt);
        const { transactionHash } = transactionReceipt;
        return {
          // XXX not sure what the distinction is supposed to be here
          wait: async () => {
            debug('cfxers:handler', fname, 'wait');
            return {
              transactionHash
            };
          }
        };
      } else {
        // @ts-ignore
        return await self._contract[fname].call(...argsConformed);
      }
    }
  }
}

export class ContractFactory {
  abi: any[]
  bytecode: string
  wallet: Wallet
  interface: ethers.utils.Interface

  constructor(abi: string|any[], bytecode: string, wallet: Wallet) {
    this.abi = (typeof abi === 'string') ? JSON.parse(abi) : abi;
    this.bytecode = bytecode;
    this.wallet = wallet;
    this.interface = new ethers.utils.Interface(this.abi);
  }

  // compare/contrast
  // https://github.com/ethers-io/ethers.js/blob/master/packages/contracts/src.ts/index.ts
  // XXX this code can return Contract directly
  // Should it wait?
  async deploy(...args: any): Promise<Contract> {
    // Note: can't bind keyword "interface"
    const {abi, bytecode: bcode, interface: iface, wallet} = this;
    const bytecode = bcode.slice(0, 2) === '0x' || bcode === '' ? bcode : '0x' + bcode;
    wallet._requireConnected();
    if (!wallet.provider) throw Error(`Impossible: provider is undefined`);
    const {conflux} = wallet.provider;

    // XXX reduce duplication with _makeHandler
    let txnOverrides: any = {};

    if (args.length === iface.deploy.inputs.length + 1) {
      txnOverrides = unbn(args.pop());
    }

    const expectedLen = iface.deploy.inputs.length;
    if (args.length !== expectedLen) {
      throw Error(`cfxers: contract deployment expected ${expectedLen} args but got ${args.length}`);
    }

    const contract = conflux.Contract({abi, bytecode});
    const from = wallet.getAddress();
    const value = BigNumber.from(0).toString();
    const txn = {from, value, ...txnOverrides};
    const argsConformed = conform(args, iface.deploy.inputs);
    debug(`cfxers:Contract.deploy`, {argsConformed, txn});

    // Note: this usage of `.call` here is because javascript is insane.
    // XXX 2021-06-07 Dan: This works for the cjs compilation target, but does it work for the other targets?
    // @ts-ignore
    const ccc = contract.constructor.call(...argsConformed);
    // debug(`cfxers:Contract.deploy`, `cfx ctc constructed`, ccc);
    // const data = ccc.data;
    // const txnDat = {...txn, data};
    // const resultP = conflux.sendTransaction(txnDat);
    const resultP = ccc.sendTransaction(txn);
    const hash = await resultP;
    const receiptP = waitReceipt(wallet.provider, hash);

    const txnRes = await conflux.getTransactionByHash(hash);
    debug(`deploy result`, {hash, txnRes});

    return new Contract(undefined, abi, wallet, receiptP, hash);
  }
  getDeployTransaction() {
    // XXX
    debug(`cfxers:getDeployTransaction`, `error`);
    throw Error(`XXX getDeployTransaction on CFX`);
  }
}

export class Wallet {
  privateKey?: string;
  account?: cfxsdk.Account;
  provider?: providers.Provider;

  constructor(privateKey?: string, provider?: providers.Provider) {
    this.privateKey = privateKey;
    if (provider) {
      this.connect(provider);
    }
  }

  connect(provider: providers.Provider) {
    if (this.provider) {
      throw Error(`Wallet already connected`);
    }
    this.provider = provider;
    if (this.privateKey) {
      this.account = this.provider.conflux.wallet.addPrivateKey(this.privateKey);
    } else {
      this.account = this.provider.conflux.wallet.addRandom();
    }
    return this;
  }

  _requireConnected() {
    if (!this.provider) {
      throw Error(`Wallet has no Provider, please call .connect()`);
    }
    if (!this.account) {
      throw Error(`Wallet has no Account, please call .connect()`);
    }
  }

  getAddress(): string {
    this._requireConnected();
    if (!this.account) throw Error(`Impossible: account is undefined`);
    return address_cfxStandardize(this.account.toString());
  }

  async sendTransaction(txn: any): Promise<{
    transactionHash: string,
    wait: () => Promise<{transactionHash: string}>,
  }> {
    this._requireConnected();
    if (!this.provider) throw Error(`Impossible: provider is undefined`);
    const from = this.getAddress();
    txn = {from, ...txn, value: (txn.value || '0').toString()};
    // This is weird but whatever
    if (txn.to instanceof Promise) {
      txn.to = await txn.to;
    }
    return _retryingSendTxn(this.provider, txn);
  }

  static createRandom(): Wallet {
    return new Wallet();
  }

  static fromMnemonic(mnemonic: string): Wallet {
    // TODO
    void(mnemonic);
    throw Error(`Account 'from mnemonic' not supported on Conflux, please use secret key`);
  }
}

// XXX This is nutty
// Remember the last epoch that a given sender has sent
// and don't try to send again until it is later than that epoch.
// Note: requires addrs to be canonicalized first.
const lastEpochSent: Record<string, number> = {};
const epochWaitLock: Record<string, boolean> = {};

// XXX implement a queue, maybe?
function tryGetLock(obj: Record<string, boolean>, k: string): boolean {
  if (!obj[k]) {
    // XXX is this actually threadsafe?
    obj[k] = true;
    return true;
  }
  return false;
}

function releaseLock(obj: Record<string, boolean>, k: string): void {
  obj[k] = false;
}

function getLastSentAt(addr: string): number {
  return lastEpochSent[addr] || -1;
}

function updateSentAt(addr: string, epoch: number) {
  lastEpochSent[addr] = Math.max(getLastSentAt(addr), epoch);
}

// Note: this relies on epochs moving on their own
// If there's ever a devnet where this is not the case,
// this will need to be adjusted.
const waitUntilSendableEpoch = async (provider: providers.Provider, addr: string): Promise<void> => {
  while (!tryGetLock(epochWaitLock, addr)) {
    // XXX fail after waiting too long?
    await Timeout.set(50);
  }
  let current: number;
  // XXX fail after waiting too long?
  while ((current = await provider.getBlockNumber()) <= getLastSentAt(addr)) {
    await Timeout.set(50); // XXX revisit how long to wait?
  }
  updateSentAt(addr, current);
  releaseLock(epochWaitLock, addr);
}

async function _retryingSendTxn(provider: providers.Provider, txnOrig: object): Promise<{
  transactionHash: string,
  wait: () => Promise<{transactionHash: string}>,
}> {
  const max_tries = 2;
  const addr = (txnOrig as any).from as string; // XXX typing
  let err: Error|null = null;
  let txnMut: any = {...txnOrig};
  for (let tries = 1; tries <= max_tries; tries++) {
    await waitUntilSendableEpoch(provider, addr);
    if (err) {
      // XXX is this still needed?
      await Timeout.set(1000); // XXX shorten this?
    }
    try {
      // Note: {...txn} because conflux is going to mutate it >=[
      txnMut = {...txnOrig};
      const transactionHashP = provider.conflux.sendTransaction(txnMut);
      const transactionHash = await transactionHashP;
      // debug(`_retryingSendTxn success`, {txnOrig, txnMut, transactionHash});
      updateSentAt(addr, txnMut.epochHeight);
      return {
        transactionHash,
        wait: async () => {
          // see: https://github.com/Conflux-Chain/js-conflux-sdk/blob/master/docs/how_to_send_tx.md#transactions-stage
          // @ts-ignore
          await transactionHashP.executed();
          return {transactionHash};
        },
      }
    } catch (e) {
      err = e;
      // debug({
      //   message: `retrying sendTxn attempt failed`,
      //   txnOrig, txnMut,
      //   e, tries, max_tries
      // });
      continue;
    }
  }
  if (!err) throw Error(`impossible: no error to throw after ${max_tries} failed attempts.`);
  throw err;
}

async function waitReceipt(provider: providers.Provider, txnHash: string): Promise<object> {
  // XXX is 5s enough time on testnet/mainnet?
  // js-conflux-sdk is willing to wait up to 5 mins before timing out, which seems a bit ridiculous
  const maxTries = 100;
  const waitMs = 50;
  for (let tries = 1; tries <= maxTries; tries++) {
    const r: any = await provider.conflux.getTransactionReceipt(txnHash);
    if (r) {
      if (r.outcomeStatus !== 0) {
        throw Error(`Transaction failed, outcomeStatus: ${r.outcomeStatus}`);
      }
      return r;
    }
    await Timeout.set(waitMs);
  }
  throw Error(`Transaction timed out after ${maxTries * waitMs} ms`);
}
