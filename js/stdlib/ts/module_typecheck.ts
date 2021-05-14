// This file only exists to check that modules satisfy an interface

import * as ETH from './ETH';
import * as ETH_compiled from './ETH_compiled';
import * as CFX from './CFX';

import { EthLike, EthLikeCompiled } from './ETH_like_interfaces';

const _ETH_compiled: EthLikeCompiled = ETH_compiled;
void(_ETH_compiled);

const _ETH: EthLike = ETH;
void(_ETH);

const _CFX: EthLike = CFX;
void(_CFX);
