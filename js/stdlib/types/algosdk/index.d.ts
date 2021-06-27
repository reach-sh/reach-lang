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
    group?: number,
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
    'apps-local-state': any
    'assets': any
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
  declare function secretKeyToMnemonic(sk: Uint8Array): string
  declare function encodeObj(obj: any): Uint8Array
  declare function generateAccount(): Wallet

  declare type CompileResult = {
    result: string,
    hash: string
  }

  declare type ARG_appArgs = Array<LogicArg>;
  declare type ARG_appAccounts = Array<string>;
  declare type ARG_foreignApps = Array<number>;
  declare type ARG_foreignAssets = Array<number>;
  declare type ARG_note = Uint8Array;
  declare type ARG_lease = Uint8Array;
  declare type ARG_rekeyTo = string;

  declare function makePaymentTxnWithSuggestedParams(
      from: Address, to: Address, amount: number,
      closeRemainderTo: Address|undefined,
      note: ARG_note, params: TxnParams
  ): Txn;
  declare function makeAssetTransferTxnWithSuggestedParams(
      from: Address, to: Address,
      closeRemainderTo: Address|undefined,
      revocationTarget: Address|undefined,
      amount: number,
      note: ARG_note,
      id: number,
      params: TxnParams
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
    numGlobalByteSlices: number,
    appArgs?: ARG_appArgs,
    accounts?: ARG_appAccounts,
    foreignApps?: ARG_appForeignApps,
    foreignAssets?: ARG_appForeignAssets,
    note?: ARG_note,
    lease?: ARG_lease,
    rekeyTo?: ARG_reKeyTo
  ): Txn;
  declare function makeApplicationOptInTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number,
    appArgs?: ARG_appArgs,
    accounts?: ARG_appAccounts,
    foreignApps?: ARG_appForeignApps,
    foreignAssets?: ARG_appForeignAssets,
    note?: ARG_note,
    lease?: ARG_lease,
    rekeyTo?: ARG_reKeyTo
  ): Txn;
  declare function makeApplicationUpdateTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number,
    approvalProgram: Uint8Array,
    clearProgram: Uint8Array,
    appArgs?: ARG_appArgs,
    accounts?: ARG_appAccounts,
    foreignApps?: ARG_appForeignApps,
    foreignAssets?: ARG_appForeignAssets,
    note?: ARG_note,
    lease?: ARG_lease,
    rekeyTo?: ARG_reKeyTo
  ): Txn;
  declare function makeApplicationDeleteTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number,
    appArgs: ARG_appArgs,
    accounts?: ARG_appAccounts,
    foreignApps?: ARG_appForeignApps,
    foreignAssets?: ARG_appForeignAssets,
    note?: ARG_note,
    lease?: ARG_lease,
    rekeyTo?: ARG_reKeyTo
  ): Txn;
  declare function makeApplicationNoOpTxn(
    from: Address,
    suggestedParams: TxnParams,
    appIndex: number,
    appArgs: ARG_appArgs,
    accounts?: ARG_appAccounts,
    foreignApps?: ARG_appForeignApps,
    foreignAssets?: ARG_appForeignAssets,
    note?: ARG_note,
    lease?: ARG_lease,
    rekeyTo?: ARG_reKeyTo
  ): Txn;
  declare function assignGroupID(
    txns: Array<Txn>
  );

  declare const OnApplicationComplete: {
    NoOpOC: OnApplicationComplete,
  };

  declare class Algodv2 {
    constructor(
      token?: string | {'X-Algo-API-Token': string},
      baseServer?: string,
      port?: string | number,
      headers?: {}, // TODO
    )

    status(): ApiCall<StatusInfo>
    statusAfterBlock(blockNumber: number): ApiCall<StatusInfo>
    pendingTransactionsInformation(): ApiCall<TxnInfo>
    pendingTransactionInformation(txId: TxId): ApiCall<TxnInfo>
    sendRawTransaction(stx_or_stxs: SignedTxn | Array<SignedTxn>): ApiCall<void>
    getTransactionParams(): ApiCall<TxnParams>
    accountInformation(addr: Address): ApiCall<AcctInfo>
    compile(code: String): ApiCall<CompileResult>
    // XXX
    dryrun(req: any): ApiCall<any>
    getApplicationByID(id: number): ApiCall<any>
  }

  declare class Indexer {
    constructor(
      token?: string,
      baseServer?: string,
      port?: string | number,
      headers?: {}, // TODO
    )

    // XXX
    lookupTransactionByID(id: any): any
    searchForTransactions(): any
  }

  // XXX
  declare const modelsv2: any
}
