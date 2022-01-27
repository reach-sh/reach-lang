import type {
  num, MaybeRep, MapOpts, LinearMap, AnyBackendTy
} from './shared_backend'; // =>
import type { BigNumber } from 'ethers'; // =>
import {
  IAccount,
  LaunchTokenOpts,
  IContract,
  IBackend,
} from './shared_impl';

export interface TypeDefs<Ty> {
  T_Null: Ty
  T_Bool: Ty
  T_UInt: Ty
  T_Bytes: (len:number) => Ty
  T_Address: Ty
  T_Contract: Ty
  T_Digest: Ty
  T_Token: Ty
  T_Object: (tyMap: {[key: string]: Ty}) => Ty
  T_Data: (tyMap: {[key: string]: Ty}) => Ty
  T_Array: (ty: Ty, size: number) => Ty
  T_Tuple: (tys: Ty[]) => Ty
  T_Struct: (nameTyPairs: [string, Ty][]) => Ty
};

export interface Stdlib_Backend_Shared_User<Ty> {
  protect: (ty: Ty, v: unknown, m?: string) => unknown
  assert: (b: boolean, message: string) => void
  Array_set: <A>(arr: A[], idx: number, val: A) => A[]
  eq: (n1: num, n2: num) => boolean
  ge: (n1: num, n2: num) => boolean
  gt: (n1: num, n2: num) => boolean
  le: (n1: num, n2: num) => boolean
  lt: (n1: num, n2: num) => boolean
  bytesEq: (s1: string, s2: string) => boolean
}

export interface Stdlib_Backend_Shared<Ty> extends Stdlib_Backend_Shared_User<Ty> {
  checkedBigNumberify: (at: string, max: BigNumber, n: any) => BigNumber
  protect: (t: any, v: unknown, ai?: string) => unknown
  Array_asyncMap: <A, B>(a: A[], f:((x:A, i:number) => Promise<B>)) => Promise<B[]>
  Array_asyncReduce: <A, B>(a: A[], b:B, f:((y:B, x:A) => Promise<B>)) => Promise<B>
  Array_zip: <A, B>(a1: A[], a2: B[]) => [A, B][]
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
};

export interface Arith {
  add: (x: num, y: num) => BigNumber
  sub: (x: num, y: num) => BigNumber
  mod: (x: num, y: num) => BigNumber
  mul: (x: num, y: num) => BigNumber
  div: (x: num, y: num) => BigNumber
};

export interface Stdlib_Backend_Base<Ty> extends Stdlib_Backend_Shared<Ty>, Arith, TypeDefs<Ty> {
  UInt_max: BigNumber
  addressEq: (addr1: unknown, addr2: unknown) => boolean
  digestEq: (x: unknown, y: unknown) => boolean
  tokenEq: (x: unknown, y: unknown) => boolean
  digest: (t: Ty, a: unknown) => string // TODO typing
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
  isBigNumber: (bn: unknown) => bn is BigNumber
  hexToBigNumber: (hex: string) => BigNumber
  bigNumberToHex: (bn: BigNumber) => string
  uintToBytes: (bn: BigNumber) => string
  parseFixedPoint: (fp: FixedPoint) => number
  numberToFixedPoint: (n: number) => FixedPoint
  parseInt: (n: {sign: boolean, i: num}) => number
  numberToInt: (n: number) => {sign: boolean, i: num}
};

// XXX
// The thing as composed by each connector
export interface Stdlib_User_Base<Ty> extends Stdlib_Backend_Shared_User<Ty>, Stdlib_User_Shared, Arith, TypeDefs<Ty> {
  isHex: (x: unknown) => boolean
  bigNumberify: (n: num|string) => BigNumber
  stringToHex: (s: string) => string
  addressEq: (addr1: string, addr2: string) => boolean
  digest: (t: any, a: unknown) => string
};

// The real thing
export interface Stdlib_User<Provider, ProviderEnv, ProviderName, Token, ContractInfo, Address, NetworkAccount, Ty extends AnyBackendTy, Backend extends IBackend<Ty>, Account extends IAccount<NetworkAccount, Backend, IContract<ContractInfo, Address, Token, Ty>, ContractInfo, Token>> extends Stdlib_User_Base<Ty>, ProviderLib<Provider, ProviderEnv, ProviderName> {
  getValidQueryWindow: () => number|true
  setValidQueryWindow: (n: number|true) => void
  getQueryLowerBound: () => BigNumber
  setQueryLowerBound: (n: number|BigNumber) => void
  connector: string
  randomUInt: () => BigNumber
  hasRandom: { random: () => BigNumber }
  hasConsoleLogger: { log: (...a: any) => void }
  balanceOf: (acc: Account, token?: Token) => Promise<BigNumber>
  minimumBalanceOf: (acc:Account) => Promise<BigNumber>
  transfer: (from: Account, to: Account, val?: BigNumber, token?: Token) => Promise<unknown>
  connectAccount: (networkAccount: NetworkAccount) => Promise<Account>
  newAccountFromSecret: (secret: string) => Promise<Account>
  newAccountFromMnemonic: (phrase: string) => Promise<Account>
  getDefaultAccount: () => Promise<Account>
  createAccount: () => Promise<Account>
  getFaucet: () => Promise<Account> // XXX
  canFundFromFaucet: () => Promise<boolean>
  fundFromFaucet: (acc: Account, balance: BigNumber) => Promise<void>
  newTestAccount: (balance: BigNumber) => Promise<Account>
  newTestAccounts: (num: number, balance: BigNumber) => Promise<Array<Account>>
  getNetworkTime: () => Promise<BigNumber>
  waitUntilTime: (time: BigNumber) => Promise<BigNumber>
  wait: (delta: BigNumber) => Promise<BigNumber>
  getNetworkSecs: () => Promise<BigNumber>
  waitUntilSecs: (secs: BigNumber) => Promise<BigNumber>
  verifyContract: (ctcInfo: ContractInfo, backend: Backend) => Promise<any>
  /** @description the display name of the standard unit of currency for the network */
  standardUnit: string
  /** @description the display name of the atomic (smallest) unit of currency for the network */
  atomicUnit: string
  parseCurrency: (amtDesc: any) => BigNumber
  minimumBalance: BigNumber
  formatCurrency: (amt: BigNumber, decimals: number) => string
  formatAddress: (acc: Account|string) => string
  unsafeGetMnemonic: (acc: Account) => string
  launchToken: (acc: Account, name: string, sym: string, opts?:LaunchTokenOpts) => any
  reachStdlib: Stdlib_Backend<Ty>
  setMinMillisBetweenRequests: (n: number) => void
  setCustomHttpEventHandler: (h: (e: any) => Promise<void>) => void
}
