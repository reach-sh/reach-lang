import type {
  num, MaybeRep, MapOpts, LinearMap, AnyBackendTy,
} from './shared_backend'; // =>
import type { BigNumber } from 'ethers'; // =>
import {
  IAccount,
  LaunchTokenOpts,
  IContract,
  IBackend,
  SetSigningMonitor,
  UIntTy,
  TransferOpts,
} from './shared_impl';

export interface TypeDefs<Ty> {
  T_Null: Ty,
  T_Bool: Ty,
  T_UInt: Ty,
  T_UInt256: Ty,
  T_Bytes: (len:number) => Ty,
  T_BytesDyn: Ty,
  T_StringDyn: Ty,
  T_Address: Ty,
  T_Contract: Ty,
  T_Digest: Ty,
  T_Token: Ty,
  T_Object: (tyMap: {[key: string]: Ty}) => Ty,
  T_Data: (tyMap: {[key: string]: Ty}) => Ty,
  T_Array: (ty: Ty, size: number) => Ty,
  T_Tuple: (tys: Ty[]) => Ty,
  T_Struct: (nameTyPairs: [string, Ty][]) => Ty,
};

export interface Stdlib_Backend_Shared_User<Ty> {
  protect: (ty: Ty, v: unknown, m?: string) => unknown,
  assert: (b: boolean, message: string) => void,
  Array_set: <A>(arr: A[], idx: number, val: A) => A[],
  eq: (n1: num, n2: num) => boolean,
  ge: (n1: num, n2: num) => boolean,
  gt: (n1: num, n2: num) => boolean,
  le: (n1: num, n2: num) => boolean,
  lt: (n1: num, n2: num) => boolean,
  eq256: (n1: num, n2: num) => boolean,
  ge256: (n1: num, n2: num) => boolean,
  gt256: (n1: num, n2: num) => boolean,
  le256: (n1: num, n2: num) => boolean,
  lt256: (n1: num, n2: num) => boolean,
  bytesEq: (s1: string, s2: string) => boolean,
  digest_xor: (x: string, y:string) => string,
  bytes_xor: (x: string, y:string) => string,
  btoiLast8: (b: string) => BigNumber,
  stringDynConcat: (s1: string, s2: string) => string,
  uintToStringDyn: (n1: num) => string,
  uintToStringDyn256: (n1: num) => string,
};

export interface Stdlib_Backend_Shared<Ty> extends Stdlib_Backend_Shared_User<Ty> {
  checkedBigNumberify: (at: string, max: BigNumber, n: any) => BigNumber
  protect: (t: any, v: unknown, ai?: string) => unknown
  Array_asyncMap: <B>(as:any[][], f:(x:any[], i:number) => Promise<B>) => Promise<B[]>
  Array_asyncReduce: <B>(as:any[][], b: B, f:((xs:any[], y:B, i:number) => Promise<B>)) => Promise<B>
  newMap: <A>(opts: MapOpts<A>) => LinearMap<A>
  mapRef: <A>(m: LinearMap<A>, f: string) => Promise<MaybeRep<A>>
  mapSet: <A>(m: LinearMap<A>, f: string, v: A) => Promise<void>
  simMapRef: <A>(sim_r: unknown, mapi: number, f: string) => Promise<MaybeRep<A>>
  simMapSet: <A>(sim_r: unknown, mapi: number, f: string, v: A) => Promise<void>,
  simMapDupe: <A>(sim_r: unknown, mapi: number, mapo: LinearMap<A>) => void,
  simTokenNew: any, // XXX
  simTokenBurn: any, // XXX
  simTokenDestroy: any, // XXX
  bytesConcat: (b1: string, b2: string) => string
  fromSome: <A>(mo:MaybeRep<A>, da:A) => A
  simContractNew: any, // XXX
};

export interface Arith {
  add: (x: num, y: num) => BigNumber
  safeAdd: (x: num, y: num) => BigNumber
  sub: (x: num, y: num) => BigNumber
  safeSub: (x: num, y: num) => BigNumber
  mod: (x: num, y: num) => BigNumber
  safeMod: (x: num, y: num) => BigNumber
  mul: (x: num, y: num) => BigNumber
  safeMul: (x: num, y: num) => BigNumber
  div: (x: num, y: num) => BigNumber
  safeDiv: (x: num, y: num) => BigNumber
  band: (x: num, y: num) => BigNumber
  bior: (x: num, y: num) => BigNumber
  bxor: (x: num, y: num) => BigNumber
  sqrt: (n1: num, n2: num) => BigNumber
  add256: (x: num, y: num) => BigNumber
  safeAdd256: (x: num, y: num) => BigNumber
  sub256: (x: num, y: num) => BigNumber
  safeSub256: (x: num, y: num) => BigNumber
  mod256: (x: num, y: num) => BigNumber
  safeMod256: (x: num, y: num) => BigNumber
  mul256: (x: num, y: num) => BigNumber
  safeMul256: (x: num, y: num) => BigNumber
  div256: (x: num, y: num) => BigNumber
  safeDiv256: (x: num, y: num) => BigNumber
  band256: (x: num, y: num) => BigNumber
  bior256: (x: num, y: num) => BigNumber
  bxor256: (x: num, y: num) => BigNumber
  sqrt256: (n1: num, n2: num) => BigNumber
  safeMuldiv: (x: num, y: num, z:num) => BigNumber
  muldiv: (x: num, y: num, z:num) => BigNumber
  cast: (from: UIntTy, to: UIntTy, x: num, truncate: boolean, chkOverflow: boolean) => BigNumber
};

export interface Stdlib_Backend_Base<Ty> extends Stdlib_Backend_Shared<Ty>, Arith, TypeDefs<Ty> {
  UInt_max: BigNumber
  ctcAddrEq: (ctc_x: unknown, addr_y: unknown) => boolean
  addressEq: (addr1: unknown, addr2: unknown) => boolean
  digestEq: (x: unknown, y: unknown) => boolean
  digest_xor: (x: string, y: string) => string
  tokenEq: (x: unknown, y: unknown) => boolean
  digest: (ts: Ty[], vs: unknown[]) => string // TODO typing
  emptyContractInfo: (number | string),
};

// XXX
export interface Stdlib_Backend<Ty> extends Stdlib_Backend_Base<Ty> {
};

// XXX
export interface Stdlib_Impl_Shared {
};

// TODO: types
export interface ProviderLib<Provider, ProviderEnv, ProviderName> {
  getProvider: () => Promise<Provider>
  setProvider: (p: Promise<Provider>) => void
  setProviderByEnv: (env: ProviderEnv) => void
  setProviderByName: (providerName: ProviderName) => void
  providerEnvByName: (providerName: ProviderName) => ProviderEnv
  setWalletFallback: (wallet:any) => void
  walletFallback: (opts:any) => any
};

type FixedPoint = { sign: boolean, i: { i: BigNumber, scale: BigNumber } }

// XXX
// The shared code
export interface Stdlib_User_Shared {
  bigNumberToNumber: (bn: BigNumber) => number
  bigNumberToBigInt: (bn: BigNumber) => bigint
  isBigNumber: (bn: unknown) => bn is BigNumber
  hexToBigNumber: (hex: string) => BigNumber
  bigNumberToHex: (bn: BigNumber) => string
  isHex: (x: unknown) => boolean
  bigNumberify: (n: num|string) => BigNumber
  stringToHex: (s: string) => string
  uintToBytes: (bn: BigNumber) => string
  parseFixedPoint: (fp: FixedPoint) => number
  numberToFixedPoint: (n: number) => FixedPoint
  parseInt: (n: {sign: boolean, i: num}) => number
  numberToInt: (n: number) => {sign: boolean, i: num}
  withDisconnect: <T>(f: () => Promise<T>) => Promise<T>
  disconnect: (t: any) => void
};

// The thing as composed by each connector
export interface Stdlib_User_Base<Ty> extends Stdlib_Backend_Shared_User<Ty>, Stdlib_User_Shared, Arith, TypeDefs<Ty> {
  addressEq: (addr1: string, addr2: string) => boolean
  digest: (ts: any[], vs: any[]) => string
};

// The real thing
export interface Stdlib_User<Provider, ProviderEnv, ProviderName, Token, ContractInfo, Address, NetworkAccount, Ty extends AnyBackendTy, Backend extends IBackend<Ty>, Contract extends IContract<ContractInfo, Address, Token, Ty>, Account extends IAccount<NetworkAccount, Backend, Contract, ContractInfo, Token>> extends Stdlib_User_Base<Ty>, ProviderLib<Provider, ProviderEnv, ProviderName> {
  getValidQueryWindow: () => number | true
  setValidQueryWindow: (n: number | true) => void
  getQueryLowerBound: () => BigNumber
  setQueryLowerBound: (n: number | BigNumber) => void
  connector: 'ALGO' | 'CFX' | 'ETH'
  randomUInt: () => BigNumber
  hasRandom: { random: () => BigNumber }
  hasConsoleLogger: { log: (...a: any) => void }
  balanceOf: (acc: Account | Address, token?: Token) => Promise<BigNumber>
  balancesOf: (acc: Account | Address, tokens: Array<Token | null>) => Promise<Array<BigNumber>>
  minimumBalanceOf: (acc: Account | Address) => Promise<BigNumber>
  transfer: (from: Account, to: Account | Address, amount: any, token?: Token, opts?: TransferOpts) => Promise<unknown>
  connectAccount: (networkAccount: NetworkAccount) => Promise<Account>
  newAccountFromSecret: (secret: string) => Promise<Account>
  newAccountFromMnemonic: (phrase: string) => Promise<Account>
  getDefaultAccount: () => Promise<Account>
  createAccount: () => Promise<Account>
  getFaucet: () => Promise<Account> // XXX
  canFundFromFaucet: () => Promise<boolean>
  fundFromFaucet: (acc: Account | Address, balance: BigNumber) => Promise<void>
  newTestAccount: (balance: BigNumber) => Promise<Account>
  newTestAccounts: (num: number, balance: BigNumber) => Promise<Array<Account>>
  getNetworkTime: () => Promise<BigNumber>
  waitUntilTime: (time: BigNumber) => Promise<BigNumber>
  wait: (delta: BigNumber) => Promise<BigNumber>
  getNetworkSecs: () => Promise<BigNumber>
  getTimeSecs: (time: BigNumber) => Promise<BigNumber>
  waitUntilSecs: (secs: BigNumber) => Promise<BigNumber>
  verifyContract: (ctcInfo: ContractInfo, backend: Backend) => Promise<any>
  /** @description the display name of the standard unit of currency for the network */
  standardUnit: string
  /** @description the display name of the atomic (smallest) unit of currency for the network */
  atomicUnit: string
  parseCurrency: (amtDesc: any) => BigNumber
  minimumBalance: BigNumber
  formatCurrency: (amt: BigNumber, decimals: number) => string
  formatAddress: (acc: Account | Address | string) => string
  formatWithDecimals: (amt: unknown, decimals: number) => string
  unsafeGetMnemonic: (acc: Account) => string
  launchToken: (acc: Account, name: string, sym: string, opts?: LaunchTokenOpts) => any
  reachStdlib: Stdlib_Backend<Ty>
  setMinMillisBetweenRequests: (n: number) => void
  setCustomHttpEventHandler: (h: (e: any) => Promise<void>) => void
  setSigningMonitor: SetSigningMonitor
  tokensAccepted: (acc: Account | Address) => Promise<Array<Token>>
  appOptedIn: (acc: Account | Address, ctc: ContractInfo) => Promise<boolean>
  contract: (bin: Backend, ctcInfo?: Promise<ContractInfo>) => Promise<Contract>,
}
