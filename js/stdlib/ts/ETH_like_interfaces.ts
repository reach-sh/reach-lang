// ****************************************************************************
// Interfaces for the various modules of ETH_like
// ****************************************************************************


// Types only!
import type { num } from './shared';
import type { BigNumber, ethers } from 'ethers';
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
  bigNumberify: any
  bigNumberToNumber: any
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

export interface StdlibBase extends SharedStdlib, Arith, TypeDefs   {
  addressEq: (addr1: unknown, addr2: unknown) => boolean
  tokenEq: (x: unknown, y: unknown) => boolean
  digest: (t: AnyETH_Ty, a: unknown) => string // TODO typing
}

export interface BackendStdlib extends StdlibBase {
  UInt_max: BigNumber
}

export interface EthLikeCompiled extends BackendStdlib {
  stdlib: BackendStdlib
  typeDefs: TypeDefs
}

// TODO: types
export interface ProviderLib {
  getProvider: any
  setProvider: any
  setProviderByEnv: any
  setProviderByName: any
  providerEnvByName: any
}

// TODO: types
export interface ReachStdlib extends StdlibBase, ProviderLib {
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
  reachStdlib: BackendStdlib
}

// EthersLike stuff .............................

export interface EthersLikeSigner {
  // TODO
  isSigner(...arg: any): boolean
}

export interface EthersLikeContractFactory {

}

export interface EthersLikeContract {
  // TODO
  [key: string]: any

  interface: ethers.utils.Interface
}

// like ethers.providers
export interface EthersLikeProviders {

}

export interface EthersLikeContractFactory {
  // TODO
  deploy(...args: any): Promise<EthersLikeContract>
  getDeployTransaction(...args: any): any
  interface: ethers.utils.Interface
}

export interface EthersLikeContractFactoryClass {
  // TODO: type args
  new (...args: any): EthersLikeContractFactory
}

export interface EthersLikeContractClass {
  // TODO: type args
  new (...args: any): EthersLikeContract
}

// like ethers
export interface EthersLike {
  Contract: EthersLikeContractClass
  ContractFactory: EthersLikeContractFactoryClass
  Wallet: EthersLikeWalletClass
  Signer: EthersLikeSigner
  providers: EthersLikeProviders
}

export interface EthersLikeWallet {
  // TODO: type args
  connect(...args: any): this
}

export interface EthersLikeWalletClass {
  new(secret: string): EthersLikeWallet
  fromMnemonic(mnemonic: string): EthersLikeWallet
  createRandom(): EthersLikeWallet
}

export interface EthLikeArgs {
  ethLikeCompiled: EthLikeCompiled
  ethers: EthersLike
  standardDigits?: number
  providerLib: ProviderLib
  isIsolatedNetwork(): boolean
  isWindowProvider(): boolean
  _getDefaultNetworkAccount(): any
  _getDefaultFaucetNetworkAccount(): any
  _verifyContractCode?: boolean
  _warnTxNoBlockNumber?: boolean
  standardUnit: string
  atomicUnit: string
}

export interface EthLikeCompiledArgs {
  T_Address: ETH_Ty<string, string>
}
