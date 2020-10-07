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
  declare type SignedTxn = Uint8Array;
  declare type SignedTxn_LSig = {
    txID: string,
    blob: SignedTxn
  }
  declare type Txn = {
    txID: () => TxIdWrapper,
    lastRound: number,
    fee: number,
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

  declare type RawAddress = Uint8Array;
  declare type DecodedAddress = {
    publicKey: RawAddress,
    checksum: Uint8Array
  }

  declare function encodeAddress(publicKey: RawAddress): Address
  declare function decodeAddress(addr: Address): DecodedAddress
  declare function mnemonicToSecretKey(mn: string): Wallet
  declare function encodeObj(obj: any): Uint8Array
  declare function generateAccount(): Wallet

  declare type CompileResult = {
    result: string,
    hash: string
  }

  declare function makePaymentTxnWithSuggestedParams(
      from: Address, to: Address, amount: number, closeRemainderTo: undefined,
      note: Uint8Array, params: TxnParams
  ): Txn;
  declare type LogicArg = Uint8Array | string;
  declare function makeLogicSig(
    program: Uint8Array,
    args: Array<LogicArg>
  ): LogicSig;
  declare function signLogicSigTransactionObject(
    txn: Txn, lsig: LogicSig
  ): SignedTxn_LSig;
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
    // XXX
    dryrun(req: any): ApiCall<any>
  }

  declare class Indexer {
    constructor(
      token?: string,
      baseServer?: string,
      port?: string | number,
      headers?: {}, // TODO
    )

    // XXX
    searchForTransactions(): any
  }

  // XXX
  declare const modelsv2: any
}
