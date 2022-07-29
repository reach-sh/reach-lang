import {
  EthersLike, EthLikeArgs,
  EthLikeCompiled, EthLikeCompiledArgs
} from './ETH_like_interfaces';
import {
  Stdlib_User,
} from './interfaces';

import { makeEthLike } from './ETH_like'
import * as ethImpl from './ETH_impl';
import type { Provider, ProviderEnv, ProviderName } from './ETH_impl';
import type { Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Account } from './ETH_like';
import * as ethers from 'ethers';
import * as shared_user from './shared_user';
import * as ETH_compiled from './ETH_compiled';
import * as ETH_compiled_impl from './ETH_compiled_impl';

const _ETH_compiled_impl: EthLikeCompiledArgs = ETH_compiled_impl;
void(_ETH_compiled_impl);

export const load = (): Stdlib_User<Provider, ProviderEnv, ProviderName, Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Account> => {
  const ethers_: EthersLike = ethers;
  const ethImpl_: EthLikeArgs<Provider, ProviderEnv, ProviderName> = ethImpl;
  const ethLike = makeEthLike(ethImpl_);
  const ETH_compiled_: EthLikeCompiled = ETH_compiled;

  const connector = 'ETH';
  return {
    ...ethers_,
    ...ethLike,
    ...ethLike.reachStdlib,
    ...shared_user,
    ...ETH_compiled_,
    connector,
  };
};
