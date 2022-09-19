import {
  EthersLike, EthLikeArgs,
  EthLikeCompiled, EthLikeCompiledArgs
} from './ETH_like_interfaces';
import {
  Stdlib_User,
} from './interfaces';

import { makeEthLike } from './ETH_like'
import * as cfxImpl from './CFX_impl';
import type { Provider, ProviderEnv, ProviderName } from './CFX_impl';
import type { Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Account, Contract } from './ETH_like';
import * as ethers from './cfxers';
import * as shared_user from './shared_user';
import * as CFX_compiled from './CFX_compiled';
import * as ETH_compiled_impl from './ETH_compiled_impl';
import { stdlibShared } from './shared_impl';

const _ETH_compiled_impl: EthLikeCompiledArgs = ETH_compiled_impl;
void(_ETH_compiled_impl);

export const load = (): Stdlib_User<Provider, ProviderEnv, ProviderName, Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Contract, Account> => {
  const ethers_: EthersLike = ethers;
  const cfxImpl_: EthLikeArgs<Provider, ProviderEnv, ProviderName> = cfxImpl;
  const ethLike = makeEthLike(cfxImpl_);
  const CFX_compiled_: EthLikeCompiled = CFX_compiled;

  const connector = 'CFX';
  return stdlibShared({
    ...ethers_,
    ...ethLike,
    ...ethLike.reachStdlib,
    ...shared_user,
    ...CFX_compiled_,
    connector,
  });
};
