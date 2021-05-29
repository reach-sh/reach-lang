import { makeEthLikeCompiled } from './ETH_like_compiled';
import type { // =>
  EthLikeCompiled
} from './ETH_like_interfaces';
import * as ethCompiledImpl from './ETH_compiled_impl';

export type { // =>
  Token, PayAmt, AnyETH_Ty
} from './ETH_like_compiled';

const ethCompiled: EthLikeCompiled = makeEthLikeCompiled(ethCompiledImpl);
// The following should be identical to CFX_compiled.ts
export const {
  stdlib,
  typeDefs,
} = ethCompiled;
export const {
  // start ...arith,
  add,
  sub,
  mul,
  div,
  mod,
  // end ...arith,

  // start ...typeDefs,
  T_Null,
  T_Bool,
  T_UInt,
  T_Bytes,
  T_Address,
  T_Digest,
  T_Token,
  T_Object,
  T_Data,
  T_Array,
  T_Tuple,
  T_Struct,
  // end ...typeDefs,

  UInt_max,
  digest,
  addressEq,
  tokenEq,
} = stdlib;
