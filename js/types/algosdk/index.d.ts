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
  declare type SignedTxn = {
    opaque: undefined // TODO
  }
  declare type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
    signTxn: (sk: SecretKey) => SignedTxn,
  }
  declare type TxnParams = {
    flatFee: boolean,
    fee: number,
    firstRound: number,
    lastRound: number,
    genesisID: number,
    genesisHash: string,
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
  declare type ApiCall<T> = {
    do(): Promise<T>,
  };

  declare function addressPublicKey(addr: Address): Uint8Array
  declare function mnemonicToSecretKey(mn: string): Wallet
  declare function encodeObj(obj: any): Uint8Array
  declare function generateAccount(): Wallet

  declare type CompileResult = {
    result: string,
    hash: string
  }

  // TODO: BigNumber not any, without breaking everything
  declare function makePaymentTxnWithSuggestedParams(
      from: Address, to: Address, amount: number, closeRemainderTo: undefined,
      note: Uint8Array, params: TxnParams
  ): Txn;
  declare type LogicArg = number | Uint8Array | string;
  declare function makeLogicSig(
    program: Uint8Array,
    args: Array<LogicArg>
  ): LogicSig;
  declare function signLogicSigTransactionObject(
    txn: Txn, lsig: LogicSig
  ): SignedTxn;
  declare function makeApplicationCreateTxn(
    from: Address,
    suggestedParams: TxnParams,
    onComplete: OnApplicationComplete,
    approvalProgram: Uint8Array,
    clearProgram: Uint8Array,
    numLocalInts: number,
    numLocalByteSlices: number,
    numGlobalInts: number,
    numGlobalByteSlices: number
  ): Txn;
  declare function makeApplicationUpdateTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number,
    approvalProgram: Uint8Array,
    clearProgram: Uint8Array
  ): Txn;
  declare function makeApplicationDeleteTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number
  ): Txn;
  declare function makeApplicationNoOpTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number
  ): Txn;
  declare function assignGroupID(
    txns: Array<Txn>
  );

  declare const OnApplicationComplete: {
    NoOpOC: OnApplicationComplete,
  };

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
    sendRawTransaction(stx_or_stxs: SignedTxn | Array<SignedTxn>): ApiCall<void>
    getTransactionParams(): ApiCall<TxnParams>
    accountInformation(addr: Address): ApiCall<AcctInfo>
    compile(code: String): ApiCall<CompileResult>
  }
}
