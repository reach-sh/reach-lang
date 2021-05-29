// Types only!
import type { // =>
  TypeDefs,
  Stdlib_Backend_Base,
  ProviderLib,
  Stdlib_Impl_Shared,
} from './interfaces';
import type { // =>
  ethers
} from 'ethers';
import type { // =>
  CBR_Val
} from './CBR';

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

export interface EthLikeCompiled extends Stdlib_Impl_Shared {
  stdlib: Stdlib_Backend_Base<AnyETH_Ty>
  typeDefs: TypeDefs
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
