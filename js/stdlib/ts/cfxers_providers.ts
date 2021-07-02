import cfxsdk from 'js-conflux-sdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import { defaultEpochTag } from './CFX_util';
import { debug } from './shared_impl';

type BigNumber = ethers.BigNumber;
type EpochNumber = cfxsdk.EpochNumber;
type Conflux = cfxsdk.Conflux;

// function epochToBlockNumber(x: Record<string, any>): Record<string, any> {
//   return {
//     blockNumber: x.epochNumber,
//     ...x,
//   };
// }

async function attachBlockNumbers(conflux: Conflux, xs: any[]): Promise<any[]> {
  async function actuallyLookup(blockHash: string): Promise<string> {
    debug(`actuallyLookup`, `block by hash query`, blockHash);
    const block = await conflux.getBlockByHash(blockHash);
    debug(`actuallyLookup`, `block by hash result`, blockHash, block);
    // @ts-ignore // XXX requires an update to js-conflux-sdk types
    return parseInt(block.blockNumber);
  };
  const cache: {[blockHash: string]: string} = {};
  async function lookup(blockHash: string): Promise<string> {
    if (!(blockHash in cache)) { cache[blockHash] = await actuallyLookup(blockHash); }
    return cache[blockHash];
  }
  async function attachBlockNumber(x: any): Promise<object> {
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
}

export function ethifyOkReceipt(receipt: any): any {
  if (receipt.outcomeStatus !== 0) {
    throw Error(`Receipt outcomeStatus is nonzero: ${receipt.outcomeStatus}`);
  }
  return {
    status: 'ok',
    ...receipt,
  }
  // return epochToBlockNumber({
  //   status: 'ok',
  //   ...receipt,
  // });
}

export function ethifyTxn(txn: any): any {
  if (txn.status !== 0) {
    throw Error(`Txn status is not 0: ${txn.status}`);
  }
  // It would appear that no eth-ification is actully necessary at this moment.
  // It might be nice to have blockNumber on here,
  // but it's not required.
  // Accomplishing that would require another API call...
  return txn;
}

// XXX bi: BigInt
function bi2bn(bi: any): BigNumber {
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
    // Arbitrarily make the user wait.
    // This is just because we tend to spam this a lot.
    // It can help to increase this to 1000 or more if you need to debug.
    await Timeout.set(50);
    const epochNumber = await this.conflux.getEpochNumber(defaultEpochTag);
    const block = await this.conflux.getBlockByEpochNumber(epochNumber, true);
    // @ts-ignore
    return parseInt(block.blockNumber);
  }

  async getBlock(which: number): Promise<any> {
    debug(`getBlock`, which);
    // @ts-ignore
    return await this.conflux.getBlockByBlockNumber(which, true);
  }

  async getTransactionReceipt(transactionHash: string): Promise<any> {
    // Arbitrarily make the user wait.
    // This is just because we tend to spam this a lot.
    // It can help to increase this to 1000 or more if you need to debug.
    await Timeout.set(1000); // XXX

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

  async getLogs(opts: {fromBlock: number, toBlock: number, address: string, topics: string[]}): Promise<any[]> {
    debug(`getLogs`, `opts`, opts);

    // XXX reduce this
    // arbitrary throttle
    await Timeout.set(1000);

    // TODO: search from newest to oldest instead of oldest to newest
    let {fromBlock, toBlock} = opts;
    // XXX start from now and go backwards
    const maxGap = 1000; // max gap is 1000 epochs so 1000 blocks is conservative
    // const startingPoint = 30000000; // an arbitrary point in the past
    const startingPoint = Math.max(toBlock - maxGap, 0); // XXX allow older XXXXXXXXXXXXXX
    fromBlock = Math.max(fromBlock, startingPoint);
    opts = {...opts, fromBlock};
    // debug(`getLogs`, `modified opts`, opts);
    if (toBlock >= fromBlock && toBlock - fromBlock > maxGap) {
      let logs: object[] = [];
      let start = fromBlock;
      let next = Math.min(start + maxGap, toBlock);
      while (start < toBlock) {
        // debug(`getLogs`, `smaller chunks`, {fromBlock, toBlock, start, next});
        const newOpts = {...opts, fromBlock: start, toBlock: next}
        const more = await this.getLogs(newOpts);
        logs = logs.concat(more);
        start = next;
        next = Math.min(start + maxGap, toBlock);
      };
      debug(`getLogs`, `loop result`, logs);
      return logs;
    }

    debug(`getLogs`, `base case`, opts);
    const max_tries = 20;
    let e: Error|null = null;
    for (let tries = 1; tries <= max_tries; tries++) {
      try {
        const logs = await this.conflux.getLogs(opts);
        debug(`getLogs`, `base case result`, logs);
        return await attachBlockNumbers(this.conflux, logs);
        // return (await this.conflux.getLogs(cfxOpts)).map(epochToBlockNumber);
      } catch (err) {
        e = err;
        // XXX be pickier about which errs we are willing to catch
        // XXX find some way to be sure more that `toEpoch` has been executed before trying again
        await Timeout.set(50);
      }
    }
    debug(`getLogs`, `error`, e);
    throw e;
  }

  async getTransaction(txnHash: string): Promise<any> {
    // @ts-ignore
    return ethifyTxn(await this.conflux.getTransactionByHash(txnHash));
  }

}

export type TransactionReceipt = any; // TODO
export type Log = any; // TODO
