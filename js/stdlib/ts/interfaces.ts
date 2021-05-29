import type { num } from './shared_backend'; // =>
import type { BigNumber } from 'ethers'; // =>

// TODO better typing on these
export interface TypeDefs {
  T_Null: any
  T_Bool: any
  T_UInt: any
  T_Bytes: any
  T_Address: any
  T_Digest: any
  T_Token: any
  T_Object: any
  T_Data: any
  T_Array: any
  T_Tuple: any
  T_Struct: any
};

export interface Stdlib_Backend_Shared_User {
  protect: any,
  assert: any,
  Array_set: any,
  eq: any,
  ge: any,
  gt: any,
  le: any,
  lt: any,
  bytesEq: any,
  digestEq: any,
}

export interface Stdlib_Backend_Shared extends Stdlib_Backend_Shared_User {
  checkedBigNumberify: any,
  protect: any,
  Array_zip: any,
  mapRef: any,
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

// TODO: types
export interface ProviderLib {
  getProvider: any
  setProvider: any
  setProviderByEnv: any
  setProviderByName: any
  providerEnvByName: any
};

// XXX
// The shared code
export interface Stdlib_User_Shared {
  isBigNumber: any
  hexToBigNumber: any
  bigNumberToHex: any
  uintToBytes: any
  parseFixedPoint: any
  parseInt: any
};

// XXX
// The thing as composed by each connector
export interface Stdlib_User_Base extends Stdlib_Backend_Shared_User, Stdlib_User_Shared, Arith, TypeDefs {
  isHex: any
  bigNumberify: any
  stringToHex: any
  addressEq: any
  digest: any
};

// TODO: types
// The real thing
export interface Stdlib_User<Ty> extends Stdlib_User_Base, ProviderLib {
  connector: string
  randomUInt: any
  hasRandom: any
  balanceOf: any
  transfer: any
  connectAccount: any
  newAccountFromSecret: any
  newAccountFromMnemonic: any
  getDefaultAccount: any
  createAccount: any
  fundFromFaucet: any
  newTestAccount: any
  getNetworkTime: any
  wait: any
  waitUntilTime: any
  verifyContract: any
  /** @description the display name of the standard unit of currency for the network */
  standardUnit: string
  /** @description the display name of the atomic (smallest) unit of currency for the network */
  atomicUnit: string
  parseCurrency: any
  minimumBalance: any
  formatCurrency: any
  formatAddress: any
  reachStdlib: Stdlib_Backend<Ty>
}

