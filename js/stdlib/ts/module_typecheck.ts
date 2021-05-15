// This file only exists to typecheck that modules satisfy an interface

import * as ETH from './ETH';
import * as ETH_compiled from './ETH_compiled';
import * as CFX from './CFX';
import * as ALGO from './ALGO';
import ethers from 'ethers';
import * as cfxers from './cfxers';
import * as ETH_impl from './ETH_impl';
import * as shared from './shared';

import { EthersLike, ReachStdlib, EthLikeArgs, EthLikeCompiled, SharedStdlib } from './ETH_like_interfaces';

const _shared: SharedStdlib = shared;
void(_shared);

const _ETH_compiled: EthLikeCompiled = ETH_compiled;
void(_ETH_compiled);

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

const _ethImpl: EthLikeArgs = ETH_impl;
void(_ethImpl);
