declare module 'algosdk' {
  declare function algosToMicroalgos(x: number): number;
  declare function microalgosToAlgos(x: number): number;

  declare type Round = number
  declare type Address = string
  declare type SecretKey = Uint8Array // length 64
  // TODO: find the proper algo terminology for Wallet
  declare type Wallet = {
    addr: Address,
    sk: SecretKey, // TODO: describe length? (64)
  }
  declare function mnemonicToSecretKey(mn: string): Wallet
  declare function encodeObj(obj: any): Uint8Array
  // TODO: BigNumber not any, without breaking everything
  declare function makePaymentTxnWithSuggestedParams(
      from: Address, to: Address, amount: number, closeRemainderTo: undefined,
      note: Uint8Array, params: TxnParams
  ): Txn;
  declare function generateAccount(): Wallet

  declare class ApiCall<T> {
    // This isn't real, don't create
    do(): Promise<T>;
  }

  declare type SignedTxn = {
    opaque: undefined // TODO
  }
  declare type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
    signTxn: (sk: SecretKey) => SignedTxn,
  }
  declare type TxnParams = {
    boop: string, // TODO
  }
  declare type StatusInfo = {
    'last-round': number,
  }
  declare type TxIdWrapper = {
    toString: () => string
  }
  declare type TxnInfo = {
    'confirmed-round': number,
  }
  declare type AcctInfo = {
    amount: number // bignumber?
  }
  declare type TxId = string;
  declare class Algodv2 {
    constructor(
      token?: string,
      baseServer?: string,
      port?: string | number,
      headers?: {}, // TODO
    )

    status(): ApiCall<StatusInfo>
    statusAfterBlock(blockNumber: number): ApiCall<StatusInfo>
    pendingTransactionInformation(txId: TxId): ApiCall<TxnInfo>
    sendRawTransactions(signedTxns: Array<SignedTxn>): ApiCall<void>
    sendRawTransaction(signedTxn: SignedTxn): ApiCall<void>
    getTransactionParams(): ApiCall<TxnParams>
    accountInformation(addr: Address): ApiCall<AcctInfo>
  }
}
