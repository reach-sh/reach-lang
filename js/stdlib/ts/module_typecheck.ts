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
import * as shared from './shared';
import ethers from 'ethers';

import { EthersLike, ReachStdlib, EthLikeArgs, EthLikeCompiled, SharedStdlib, EthLikeCompiledArgs } from './ETH_like_interfaces';

const _shared: SharedStdlib = shared;
void(_shared);

const _ETH_compiled: EthLikeCompiled = ETH_compiled;
void(_ETH_compiled);

const _CFX_compiled: EthLikeCompiled = CFX_compiled;
void(_CFX_compiled);

const _ETH: ReachStdlib = ETH;
void(_ETH);

const _CFX: ReachStdlib = CFX;
void(_CFX);

const _ALGO: ReachStdlib = ALGO;
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
