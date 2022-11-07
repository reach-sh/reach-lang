// This file immitates the ethers.js API
import cfxsdk from '@reach-sh/js-conflux-sdk';
import Timeout from 'await-timeout';
const { format } = cfxsdk;
import { ethers } from 'ethers';
import { ParamType } from '@ethersproject/abi';
const { BigNumber, utils } = ethers;
export { BigNumber, utils };
import { address_cfxStandardize, defaultEpochTag } from './CFX_util';
import { debug, j2s, protectSecretKey, SecretKeyInput } from './shared_impl';
import { T_Address } from './CFX_compiled_impl';

type BigNumber = ethers.BigNumber;
type EpochNumber = cfxsdk.EpochNumber;
type Conflux = cfxsdk.Conflux;
export type TransactionReceipt = any; // TODO
export type Log = any; // TODO

export type TransactionResponse = {
  transactionHash: string,
  wait: () => Promise<TransactionReceipt>,
};

const attachBlockNumbers = async (conflux: Conflux, xs: any[]): Promise<any[]> => {
  const actuallyLookup = async (blockHash: string): Promise<string> => {
    debug(`actuallyLookup`, {blockHash});
    const block = await conflux.getBlockByHash(blockHash);
    debug(`actuallyLookup`, {blockHash}, 'res', block);
    // @ts-ignore // XXX requires an update to js-conflux-sdk types
    return parseInt(block.blockNumber);
  };
  const cache: {[blockHash: string]: string} = {};
  const lookup = async (blockHash: string): Promise<string> => {
    if (!(blockHash in cache)) { cache[blockHash] = await actuallyLookup(blockHash); }
    return cache[blockHash];
  }
  const attachBlockNumber = async (x: any): Promise<object> => {
    if (x.blockNumber) {
      return x;
    } else if (x.blockHash) {
      const blockHash: string = x.blockHash;
      const blockNumber = await lookup(blockHash);
      return {...x, blockNumber};
    } else {
      throw Error(`No blockNumber or blockHash on log: ${Object.keys(x)}`);
    }
  }
  const out: any[] = [];
  // reduce the # of requests by doing them serially rather than in parallel
  for (const i in xs) { out[i] = await attachBlockNumber(xs[i]); }
  return out;
};

const ethifyOkReceipt = (receipt: any): any => {
  if (receipt.outcomeStatus !== 0) {
    throw Error(`Receipt outcomeStatus is nonzero: ${receipt.outcomeStatus}`);
  }
  return {
    status: 'ok',
    ...receipt,
  }
};

const ethifyTxn = (txn: any): any => {
  if (txn.status !== 0) {
    throw Error(`Txn status is not 0: ${txn.status}`);
  }
  // It would appear that no eth-ification is actully necessary at this moment.
  // It might be nice to have blockNumber on here,
  // but it's not required.
  // Accomplishing that would require another API call...
  return txn;
};

export namespace providers {

// XXX bi: BigInt
const bi2bn = (bi: any): BigNumber => {
  return ethers.BigNumber.from(bi.toString());
}

export class Provider {
  conflux: Conflux;
  constructor(conflux: Conflux) {
    this.conflux = conflux;
  }

  async getBalance(address: string, epochNumber?: EpochNumber): Promise<BigNumber> {
    return bi2bn(await this.conflux.getBalance(address, epochNumber));
  }

  async getBlockNumber(): Promise<number> {
    const epochNumber = await this.conflux.getEpochNumber(defaultEpochTag);
    const block = await this.conflux.getBlockByEpochNumber(epochNumber, true);
    // @ts-ignore
    debug('getBlockNumber', epochNumber, block.epochNumber, block.blockNumber);
    // @ts-ignore
    return parseInt(block.blockNumber);
  }

  async getBlock(which: number): Promise<any> {
    debug(`getBlock`, which);
    // @ts-ignore
    return await this.conflux.getBlockByBlockNumber(which, true);
  }

  async getTransactionReceipt(transactionHash: string): Promise<TransactionReceipt> {
    const r = await this.conflux.getTransactionReceipt(transactionHash);
    if (!r) return r;
    const [rbn] = await attachBlockNumbers(this.conflux, [r]);
    return ethifyOkReceipt(rbn);
  }

  async getCode(address: string, defaultEpoch: EpochNumber | undefined = undefined): Promise<string> {
    return await this.conflux.getCode(address, defaultEpoch);
  };

  on(...argz: any) {
    void(argz);
    throw Error(`on not yet implemented`);
    // XXX
  }

  off(...argz: any) {
    void(argz);
    throw Error(`off not yet implemented`);
    // XXX
  }

  async getLogs(iopts: object): Promise<any[]> {
    const opts = iopts as {fromBlock: number};
    // {fromBlock: number, toBlock: number, address: string, topics: string[]}
    debug(`getLogs`, `opts`, opts);
    if ( opts.fromBlock === 0 ) {
      opts.fromBlock = 1;
      debug(`getLogs`, `opts`, opts);
    }
    const logs = await this.conflux.getLogs(opts);
    debug(`getLogs`, `result`, logs);
    const alogs = await attachBlockNumbers(this.conflux, logs);
    debug(`getLogs`, `aresult`, alogs);
    return alogs;
  }

  async getTransaction(txnHash: string): Promise<any> {
    // @ts-ignore
    return ethifyTxn(await this.conflux.getTransactionByHash(txnHash));
  }

  async waitForTransaction(txnHash: string): Promise<TransactionReceipt> {
    const dhead = `waitForTransaction`;
    let r: any = undefined;
    let howMany = 0;
    while (! r) {
      if ( howMany++ > 0 ) {
        await Timeout.set(500);
      }
      debug(dhead, txnHash);
      r = await this.getTransactionReceipt(txnHash);
      debug(dhead, txnHash, r);
    }
    if (r.outcomeStatus !== 0) {
      throw Error(`Transaction failed, outcomeStatus: ${r.outcomeStatus}`);
    }
    return r;
  }
}

};

// Recursively stringify BigNumbers
const unbn = (arg: any): any => {
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

const booleanize = (arg: any): boolean => {
  if (typeof arg === 'boolean') return arg;
  if (typeof arg === 'number') return arg !== 0;

  // I don't quite understand why bools get represented this way sometimes, but they do.
  if (Array.isArray(arg) && arg.length === 1) return booleanize(arg[0]);

  // XXX handle more stuff
  throw Error(`don't know how to booleanize '${arg}': ${typeof arg}`);
}

const conform = (args: any[], tys: ParamType[]): any[] => {
  // XXX find a better way to do this stuff.
  args = unbn(args);
  if (Array.isArray(args)) {
    if (args.length !== tys.length) {
      debug(`conform`, `err`, {args, tys});
      throw Error(`impossible: number of args (${args.length}) does not match number of tys (${tys.length})`);
    }
    for (const i in tys) {
      const ty = tys[i].type;
      if (ty === 'tuple') {
        args[i] = conform(args[i], tys[i].components);
      } else if (ty === 'bool') {
        args[i] = booleanize(args[i]);
      } else if (ty === 'address') {
        args[i] = T_Address.munge(T_Address.canonicalize(args[i]));
      } else {
        // XXX handle more stuff
        debug(`conform untouched:`, args[i], tys[i])
      }
    }
  }
  return args;
}

const prepForConfluxPortal = (txnOrig: any): any => {
  const hexStringify = (n: any) => '0x' + BigInt(n || '0').toString(16);
  const txn = {...txnOrig};

  // value should always be present
  txn.value = hexStringify(txnOrig.value);

  // These fields are transformed if present
  // TODO: is it safe just to turn all number fields into hex strings?
  // Where is the "real" Conflux Portal source code to check this?
  for (const field of ['storageLimit', 'gas', 'nonce']) {
    if (txn[field] !== undefined) txn[field] = hexStringify(txnOrig[field]);
  }

  return txn;
}

const addEstimates = async (cfx:any, txn:any): Promise<any> => {
  const dhead = 'addEstimates';
  debug(dhead, `1: start:`, txn);
  type stringy = {toString: () => string} | undefined;
  type Num = BigInt;
  const numy = (n: stringy): Num => BigInt(n?.toString() || '0');
  const f = (xf:string): Num => {
    const x = txn[xf];
    delete txn[xf];
    return numy(x);
  };
  let gas: Num = f("gas");
  let storage: Num = f("storageLimit");
  debug(dhead, `2:  orig:`, { gas, storage });

  let est: {gasUsed?: stringy, storageCollateralized?: stringy} | undefined = undefined;
  let est_err = undefined;
  try {
    const n = await cfx.getNextNonce(txn.from);
    txn.nonce = n;
    debug(dhead, `n:nonce:`, {n});
  } catch (e) {
    debug(dhead, `n:nonce:`, {e});
  }
  try {
    est = await cfx.estimateGasAndCollateral(txn);
  } catch (e) {
    est_err = e;
  }
  debug(dhead, `3:   est:`, { est, est_err });
  if ( est ) {
    const g = (x:any, y:any) => ((y > x) ? y : x);
    gas = g(gas, numy(est?.gasUsed));
    storage = g(storage, numy(est?.storageCollateralized));
  }
  debug(dhead, `4: eused:`, { gas, storage });
  if ( storage === undefined || storage === numy(0) ) {
    storage = numy(2048);
  }
  debug(dhead, `5:  non0:`, { gas, storage });

  const h = (x:any, y:any) => numy(format.big(x).times(y).toFixed(0));
  gas = h(gas, cfx.defaultGasRatio);
  storage = h(storage, cfx.defaultStorageRatio);
  debug(dhead, `6: ratio:`, { gas, storage });

  let gasu: Num | undefined = gas;
  if ( gas === numy('0') ) {
    gasu = undefined;
  }
  debug(dhead, `7:   und:`, { gasu, storage });

  txn.gas = gasu?.toString();
  txn.storageLimit = storage.toString();
  return txn;
};

export class Signer {
  static isSigner(x: any) {
    // XXX
    return x && x.sendTransaction instanceof Function;
  }
}

interface IContract {
  [key: string]: any
}

// compare to ethers.Contract
export class Contract implements IContract {
  [k: string]: any
  _abi: any[]
  _wallet: IWallet
  _receiptP?: Promise<any>
  _contract: cfxsdk.Contract
  address?: string
  deployTransaction: TransactionResponse
  interface: ethers.utils.Interface
  constructor(address: string|null|undefined, abi: string|any[], wallet: IWallet, receiptP?: Promise<any>, transactionHash?: string) {
    this.address = address || undefined;
    const blacklist = Object.keys(this).filter((s) => s[0] === '_');
    this._abi = (typeof abi === 'string') ? JSON.parse(abi) : abi;
    this._wallet = wallet;
    this._receiptP = receiptP;
    // @ts-ignore // ???
    this._contract = this._wallet.provider.conflux.Contract({
      abi: this._abi, address: this.address,
    });
    const self = this;
    this.deployTransaction = {
      // @ts-ignore
      transactionHash,
      wait: async () => {
        debug(`cfxers:Contract.wait`, `start`);
        if (!receiptP) {
          throw Error(`No receipt promise to wait on`);
        }
        const r = await self._receiptP;
        debug(`cfxers:Contract.wait`, `got receipt`, r);
        const rcc = address_cfxStandardize(r.contractCreated);
        if (self.address && self.address !== rcc) {
          throw Error(`Impossible: ctc addresses don't match: ${self.address} vs ${rcc}`);
        }
        self.address = self.address || rcc;
        const rth = r.transactionHash;
        const dt = self.deployTransaction;
        if (dt.transactionHash && dt.transactionHash !== rth) {
          throw Error(`Impossible: txn hashes don't match: ${dt.transactionHash} vs ${rth}`);
        }
        dt.transactionHash = rth;
        return ethifyOkReceipt(r);
      },
    };
    this.interface = new ethers.utils.Interface(this._abi);
    for (const item of this._abi) {
      if (item.type === 'function') {
        if (!blacklist.includes(item.name) && item.name !== 'address' && item.name !== 'deployTransaction' && item.name !== 'interface') {
          this[item.name] = this._makeHandler(item);
        }
      }
    }
  }

  _makeHandler(abiFn: any): any {
    const {_wallet, interface: iface} = this;
    const fname: string = abiFn.name;
    const mut: string = abiFn.stateMutability;
    const from = _wallet.getAddress();
    const self = this;
    // XXX this should always be safe but maybe error handling around it just in case?
    // XXX handle the case where the same method name can have multiple input sizes/types?
    const inputs = iface.fragments.filter((x) => x.name == fname)[0].inputs;
    return async (...args: any) => {
      debug(`cfxers:handler`, fname, 'call', {args});
      let txn: any = {from, value: '0'};
      if (args.length === inputs.length + 1) {
        txn = unbn(args.pop());
        txn = {from, ...txn, value: (txn.value || '0').toString()};
      }
      args = unbn(args);
      if ( txn.gasLimit !== undefined ) {
        txn.gas = txn.gasLimit;
      }
      delete txn.gasLimit;
      debug(`cfxers:handler`, fname, 'txn', { txn, args});
      const argsConformed = conform(args, inputs);
      debug(`cfxers:handler`, fname, 'conform', argsConformed);

      if (mut !== 'view' && mut !== 'pure') {
        debug(`cfxers:handler`, fname, `waitable`);
        // Note: this usage of `.call` here is because javascript is insane.
        // XXX 2021-06-14 Dan: This works for the cjs compilation target, but does it work for the other targets?
        // @ts-ignore
        const cfc = self._contract[fname].call(...argsConformed);
        debug(`cfxers:handler`, fname, `cfc`, cfc);
        let {to, data} = cfc; // ('to' is just ctc address)
        to = to || self.address;
        txn = { ...txn, to, data};
        // @ts-ignore
        txn = await addEstimates(this._wallet.provider.conflux, txn);
        debug(`cfxers:handler`, fname, `txn`, txn);
        return await _wallet.sendTransaction(txn);
      } else {
        debug(`cfxers:handler`, fname, 'view')
        // In this case it doesn't return something with `wait`, it just
        // returns the result. Weird design choice, ethers. =/
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
    const {abi, wallet} = this;
    debug(`deploy`, {wallet});
    wallet._requireConnected();
    if (!wallet.provider) throw Error(`Impossible: provider is undefined`);
    const {conflux} = wallet.provider;

    const deployTxn = this.getDeployTransaction(...args);
    const resultP = wallet.sendTransaction(deployTxn);
    const hash = (await resultP).transactionHash;
    const receiptP = wallet.provider.waitForTransaction(hash);

    const txnRes = await conflux.getTransactionByHash(hash);
    debug(`deploy result`, {hash, txnRes});

    return new Contract(undefined, abi, wallet, receiptP, hash);
  };

  // XXX Unlike ethers, this requires having a wallet
  getDeployTransaction(...args: any): any {
    // Note: can't bind keyword "interface"
    const {abi, bytecode: bcode, interface: iface, wallet} = this;
    debug(`getDeployTransaction`, {wallet});
    const bytecode = bcode.slice(0, 2) === '0x' || bcode === '' ? bcode : '0x' + bcode;
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
    const data = ccc.data;
    return {...txn, data};
  }
}

export interface IWallet {
  provider?: providers.Provider
  connect: (provider: providers.Provider) => this
  getAddress(): string
  sendTransaction(txn: any): Promise<TransactionResponse>
}

export interface CP {
  enable: () => Promise<string[]>
  sendAsync: any // TODO
}

export class BrowserWallet implements IWallet {
  cp: CP
  address: string
  provider?: providers.Provider

  // Call await cp.enable() before this
  constructor(cp: CP, address: string, provider?: providers.Provider) {
    this.cp = cp;
    this.address = address_cfxStandardize(address);
    this.provider = provider; // XXX just use cp?
  }

  connect(provider: providers.Provider): this {
    if (this.provider) {
      throw Error(`impossible: BrowserWallet already connected`);
    }
    this.provider = provider;
    return this;
  }
  _requireConnected() {
    if (!this.provider) {
      throw Error(`Wallet has no Provider, please call .connect()`);
    }
  }

  getAddress(): string { return this.address; }

  async sendTransaction(txnOrig: any): Promise<TransactionResponse> {
    this._requireConnected();
    const {provider, address: from} = this;
    if (!provider) throw Error(`Impossible: provider is undefined`);
    const txn = prepForConfluxPortal({...txnOrig, from});
    const { value } = txn;
    const data: {result: string} = await new Promise((resolve, reject) => this.cp.sendAsync({
      from, value,
      method: 'cfx_sendTransaction',
      params: [txn],
    }, (err: unknown, data: {result: string}) => {
      if (err) reject(err); else resolve(data);
    }));
    debug('sendTransaction', { txn, data });
    if (!data) { throw Error(`No data returned from ConfluxPortal.sendAsync`); }
    const transactionHash = data.result;
    return {
      transactionHash,
      wait: () => provider.waitForTransaction(transactionHash)
    };
  }
}

// Because Conflux doesn't like it when you add the same thing twice
const accsByPk: Record<string, cfxsdk.Account> = {};
function addAcc(conflux: Conflux, privateKey: string) {
  let acc = accsByPk[privateKey];
  if (!acc) {
    acc = conflux.wallet.addPrivateKey(privateKey);
    accsByPk[privateKey] = acc;
  }
  return acc;
}

export class Wallet implements IWallet {
  privateKey?: string;
  account?: cfxsdk.Account;
  provider?: providers.Provider;
  _mnemonic?: () => {phrase: string};

  constructor(privateKey: SecretKeyInput, provider?: providers.Provider, mnem?: any) {
    this.privateKey = '0x' + Buffer.from(protectSecretKey(privateKey, 32)).toString('hex');
    if (mnem) {
      this._mnemonic = () => ({phrase: mnem});
    }
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
      this.account = addAcc(this.provider.conflux, this.privateKey);
    } else {
      throw Error(`no privateKey given`);
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

  async sendTransaction(txn: any): Promise<TransactionResponse> {
    this._requireConnected();
    const p = this.provider;
    if (!p) throw Error(`Impossible: provider is undefined`);
    const from = this.getAddress();
    txn = {from, ...txn, value: (txn.value || '0').toString()};
    txn = await addEstimates(p.conflux, txn);
    // This is weird but whatever
    if (txn.to instanceof Promise) {
       txn.to = await txn.to;
    }
    const dhead = `retryingSendTxn`;
    let howMany = 0;
    while ( true ) {
      if ( howMany++ > 0 ) {
        await Timeout.set(500);
      }
      debug(dhead, `attempt`, howMany, txn);
      // Note: {...txn} because conflux is going to mutate it >=[
      const txnMut = {...txn};
      try {
        const th = await p.conflux.sendTransaction(txnMut);
        debug(dhead, `sent`, {txn, txnMut, th});
        let got: {blockHash?: string}|null = null;
        let howMany = 0;
        while ( ! ( got && got.blockHash ) ) {
          if ( howMany++ > 2 * 60 * 5 ) {
            throw Error(`${dhead} timeout in mining ${th}`);
          }
          debug(dhead, 'get', howMany, th);
          await Timeout.set(500);
          // @ts-ignore
          got = await p.conflux.getTransactionByHash(th);
        }
        return {
          ...got,
          transactionHash: th,
          wait: () => p.waitForTransaction(th)
        }
      } catch (e:any) {
        const es = j2s(e);
        debug(dhead, `err`, { txn, e, es });
        //if ( es.includes("stale nonce") || es.includes("same nonce") || es.includes('tx already exist') ) {
        //  debug(dhead, `nonce error`);
        if ( e.code === -32077 ) {
          debug(dhead, 'catchingUp');
        } else {
          throw e;
        }
      }
    }
  }

  static createRandom(): Wallet {
    const mnem = ethers.Wallet.createRandom()._mnemonic().phrase;
    return Wallet.fromMnemonic(mnem);
  }

  static fromMnemonic(mnemonic: string, provider?: providers.Provider): Wallet {
    const sk = ethers.Wallet.fromMnemonic(mnemonic)._signingKey().privateKey;
    return new Wallet(sk, provider, mnemonic);
  }
};
