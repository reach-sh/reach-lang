// Interfaces for the various modules of ETH_like

// Types only!
import type { num } from './shared';
import type { BigNumber } from 'ethers';
import type { CBR_Val } from './CBR';

// BV = backend value
// NV = net value
export type ETH_Ty<BV extends CBR_Val, NV> =  {
  name: string,
  // ty: CBR.ReachTy,
  defaultValue: BV,
  // TODO: rename.
  // * canonicalize -> user2cbr
  // * munge/unmunge -> cbr2net/net2cbr
  canonicalize: (uv: unknown) => BV,
  munge: (bv: BV) => NV,
  unmunge: (nv: NV) => BV,
  /** @description describes the shape of the munged value */
  paramType: string,
}

export type AnyETH_Ty = ETH_Ty<CBR_Val, any>;


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
}

// TODO revisit which of these should actually be exported/exposed
export interface SharedStdlib {
  getViewsHelper: any
  deferContract: any
  truthyEnv: any
  envDefault: any
  setDEBUG: any
  getDEBUG: any
  debug: any,
  assert: any,
  isBigNumber: any,
  checkedBigNumberify: any,
  protect: any,
  isHex: any,
  hexToString: any,
  stringToHex: any,
  makeDigest: any,
  hexToBigNumber: any,
  uintToBytes: any,
  bigNumberToHex: any,
  bytesEq: any,
  digestEq: any,
  makeRandom: any,
  eq: any,
  makeArith: any,
  ge: any,
  gt: any,
  le: any,
  lt: any,
  argsSlice: any,
  argsSplit: any,
  Array_set: any,
  Array_zip: any,
  mapRef: any,
  objectMap: any,
  mkAddressEq: any,
  parseFixedPoint: any,
  parseInt: any,
}

export interface Arith {
  add: (x: num, y: num) => BigNumber
  sub: (x: num, y: num) => BigNumber
  mod: (x: num, y: num) => BigNumber
  mul: (x: num, y: num) => BigNumber
  div: (x: num, y: num) => BigNumber
}


export interface EthLikeBackendStdlib extends SharedStdlib, Arith, TypeDefs {
  addressEq: (addr1: unknown, addr2: unknown) => boolean
  tokenEq: (x: unknown, y: unknown) => boolean
  digest: (t: AnyETH_Ty, a: unknown) => string // TODO typing
  UInt_max: BigNumber
}

export interface EthLikeCompiled extends EthLikeBackendStdlib {
  stdlib: EthLikeBackendStdlib
  typeDefs: TypeDefs
}


// TODO: types
export interface EthLike extends EthLikeCompiled {
  getProvider: any
  setProvider: any
  randomUInt: any
  hasRandom: any
  setProviderByEnv: any
  setProviderByName: any
  providerEnvByName: any
  balanceOf: any
  transfer: any
  connectAccount: any
  newAccountFromSecret: any
  newAccountFromMnemonic: any
  getDefaultAccount: any
  getFaucet: any
  setFaucet: any
  createAccount: any
  fundFromFaucet: any
  newTestAccount: any
  getNetworkTime: any
  wait: any
  waitUntilTime: any
  verifyContract: any
  standardUnit: string
  atomicUnit: string
  parseCurrency: any
  minimumBalance: any
  formatCurrency: any
  formatAddress: any
  reachStdlib: EthLikeBackendStdlib
}
