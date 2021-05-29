// This file only exists to typecheck that modules satisfy an interface

import * as ALGO from './ALGO';
import * as CFX from './CFX';
import * as CFX_compiled from './CFX_compiled';
import * as CFX_compiled_impl from './CFX_compiled_impl';
import * as CFX_impl from './CFX_impl';
import * as ETH from './ETH';
import * as ETH_compiled from './ETH_compiled';
import * as ETH_compiled_impl from './ETH_compiled_impl';
import * as ETH_impl from './ETH_impl';
import * as cfxers from './cfxers';
import * as shared_backend from './shared_backend';
import * as shared_user from './shared_user';
import ethers from 'ethers';

import {
  Stdlib_User,
  Stdlib_User_Shared,
  Stdlib_Backend_Shared,
} from './interfaces';
import {
  EthersLike, EthLikeArgs,
  EthLikeCompiled, EthLikeCompiledArgs
} from './ETH_like_interfaces';

const _shared_backend: Stdlib_Backend_Shared = shared_backend;
void(_shared_backend);

const _shared_user: Stdlib_User_Shared = shared_user;
void(_shared_user);

const _ETH_compiled: EthLikeCompiled = ETH_compiled;
void(_ETH_compiled);

const _CFX_compiled: EthLikeCompiled = CFX_compiled;
void(_CFX_compiled);

const _ETH: Stdlib_User<any> = ETH;
void(_ETH);

const _CFX: Stdlib_User<any> = CFX;
void(_CFX);

const _ALGO: Stdlib_User<any> = ALGO;
void(_ALGO)

const _ethers: EthersLike = ethers;
void(_ethers);

const _cfxers: EthersLike = cfxers;
void(_cfxers);

const _ETH_impl: EthLikeArgs = ETH_impl;
void(_ETH_impl);

const _CFX_impl: EthLikeArgs = CFX_impl;
void(_CFX_impl);

const _ETH_compiled_impl: EthLikeCompiledArgs = ETH_compiled_impl;
void(_ETH_compiled_impl);

const _CFX_compiled_impl: EthLikeCompiledArgs = CFX_compiled_impl;
void(_CFX_compiled_impl);
