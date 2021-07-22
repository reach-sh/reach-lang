import type { num, MaybeRep } from './shared_backend'; // =>
import type { BigNumber } from 'ethers'; // =>

// TODO better typing on these
type Ty = any

export interface TypeDefs {
  T_Null: Ty
  T_Bool: Ty
  T_UInt: Ty
  T_Bytes: Ty
  T_Address: Ty
  T_Digest: Ty
  T_Token: Ty
  T_Object: (tyMap: {[key: string]: Ty}) => Ty
  T_Data: (tyMap: {[key: string]: Ty}) => Ty
  T_Array: (ty: Ty, size: number) => Ty
  T_Tuple: (tys: Ty[]) => Ty
  T_Struct: (nameTyPairs: [string, Ty][]) => Ty
};

export interface Stdlib_Backend_Shared_User {
  protect: (ty: Ty, v: unknown, m?: string) => unknown
  assert: (b: boolean, message: string) => void
  Array_set: <A>(arr: A[], idx: number, val: A) => A[]
  eq: (n1: num, n2: num) => boolean
  ge: (n1: num, n2: num) => boolean
  gt: (n1: num, n2: num) => boolean
  le: (n1: num, n2: num) => boolean
  lt: (n1: num, n2: num) => boolean
  bytesEq: (s1: string, s2: string) => boolean
  digestEq: (d1: string, d2: string) => boolean
}

export interface Stdlib_Backend_Shared extends Stdlib_Backend_Shared_User {
  checkedBigNumberify: (at: string, max: BigNumber, n: any) => BigNumber
  protect: (t: any, v: unknown, ai?: string) => unknown
  Array_zip: <A, B>(a1: A[], a2: B[]) => [A, B][]
  mapRef: <A>(m: {[key: string]: A}, f: string) => MaybeRep<A>
  simMapRef: (sim_r: unknown, mapi: number, f: string) => MaybeRep<unknown>
  simMapSet: (sim_r: unknown, mapi: number, f: string, v: unknown) => unknown
  simMapDupe: (sim_r: unknown, mapi: number, mapo: unknown) => void,
  simTokenNew: any, // XXX
  simTokenBurn: any, // XXX
  simTokenDestroy: any, // XXX
  bytesConcat: (b1: string, b2: string) => string
};

export interface Arith {
  add: (x: num, y: num) => BigNumber
  sub: (x: num, y: num) => BigNumber
  mod: (x: num, y: num) => BigNumber
  mul: (x: num, y: num) => BigNumber
  div: (x: num, y: num) => BigNumber
};

export interface Stdlib_Backend_Base<Ty> extends Stdlib_Backend_Shared, Arith, TypeDefs {
  UInt_max: BigNumber
  addressEq: (addr1: unknown, addr2: unknown) => boolean
  tokenEq: (x: unknown, y: unknown) => boolean
  digest: (t: Ty, a: unknown) => string // TODO typing
};

// XXX
export interface Stdlib_Backend<Ty> extends Stdlib_Backend_Base<Ty> {
};

// XXX
export interface Stdlib_Impl_Shared {
};

// TODO: per-stddlib Provider type
// TODO: per-stdlib env types
// TODO: per-stdlib providerName union of string literals type
type Provider = any
type ProviderEnv = any // Record<string, string>
type ProviderName = any

// TODO: types
export interface ProviderLib {
  getProvider: () => Provider|Promise<Provider>
  setProvider: (p: Provider|Promise<Provider>) => void
  setProviderByEnv: (env: ProviderEnv) => void
  setProviderByName: (providerName: ProviderName) => void
  providerEnvByName: (providerName: ProviderName) => ProviderEnv
  getSignStrategy: () => string
  setSignStrategy: (ss: string) => void
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
export interface Stdlib_User_Base extends Stdlib_Backend_Shared_User, Stdlib_User_Shared, Arith, TypeDefs {
  isHex: (x: unknown) => boolean
  bigNumberify: (n: num|string) => BigNumber
  stringToHex: (s: string) => string
  addressEq: (addr1: string, addr2: string) => boolean
  digest: (t: any, a: unknown) => string
};

// TODO: types for
type NetworkAccount = any
type Acc = any
type Token = any // ETH/CFX: string, ALGO: num
type CtcInfo = any
type Backend = any

// The real thing
export interface Stdlib_User<Ty> extends Stdlib_User_Base, ProviderLib {
  connector: string
  randomUInt: () => BigNumber
  hasRandom: { random: () => BigNumber }
  hasConsoleLogger: { log: (...a: any) => void }
  balanceOf: (acc: Acc, token?: Token) => Promise<BigNumber>
  transfer: (from: Acc, to: Acc, val?: BigNumber, token?: Token) => Promise<unknown>
  connectAccount: (networkAccount: NetworkAccount) => Promise<Acc>
  newAccountFromSecret: (secret: string) => Promise<Acc>
  newAccountFromMnemonic: (phrase: string) => Promise<Acc>
  getDefaultAccount: () => Promise<Acc>
  createAccount: () => Promise<Acc>
  getFaucet: () => Promise<Acc> // XXX
  fundFromFaucet: (acc: Acc, balance: BigNumber) => Promise<void>
  newTestAccount: (balance: BigNumber) => Promise<Acc>
  getNetworkTime: () => Promise<BigNumber>
  wait: (timeDelta: BigNumber) => Promise<BigNumber>
  waitUntilTime: (time: BigNumber) => Promise<BigNumber>
  verifyContract: (ctcInfo: CtcInfo, backend: Backend) => Promise<any>
  /** @description the display name of the standard unit of currency for the network */
  standardUnit: string
  /** @description the display name of the atomic (smallest) unit of currency for the network */
  atomicUnit: string
  parseCurrency: (amtDesc: any) => BigNumber
  minimumBalance: BigNumber
  formatCurrency: (amt: BigNumber, decimals: number) => string
  formatAddress: (addr: string) => string
  reachStdlib: Stdlib_Backend<Ty>
}
