import cfxsdk from 'js-conflux-sdk';
import { ethers } from 'ethers';
import Timeout from 'await-timeout';
import { defaultEpochTag } from './CFX_util';

type BigNumber = ethers.BigNumber;
type EpochNumber = cfxsdk.EpochNumber;
type Conflux = cfxsdk.Conflux;

function epochToBlockNumber(x: Record<string, any>): Record<string, any> {
  return {
    blockNumber: x.epochNumber,
    ...x,
  };
}

export function ethifyOkReceipt(receipt: any): any {
  if (receipt.outcomeStatus !== 0) {
    throw Error(`Receipt outcomeStatus is nonzero: ${receipt.outcomeStatus}`);
  }
  return epochToBlockNumber({
    status: 'ok',
    ...receipt,
  });
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
    return await this.conflux.getEpochNumber(defaultEpochTag);
  }

  async getTransactionReceipt(transactionHash: string): Promise<any> {
    const r = await this.conflux.getTransactionReceipt(transactionHash);
    return ethifyOkReceipt(r);
  }

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
    const cfxOpts = {
      fromEpoch: opts.fromBlock,
      toEpoch: opts.toBlock,
      address: opts.address,
      topics: opts.topics,
    };
    return (await this.conflux.getLogs(cfxOpts)).map(epochToBlockNumber);
  }

  async getTransaction(txnHash: string): Promise<any> {
    // @ts-ignore
    return ethifyTxn(await this.conflux.getTransactionByHash(txnHash));
  }

}

export type TransactionReceipt = any; // TODO
export type Log = any; // TODO
