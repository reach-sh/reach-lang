import { makeCompiledStdlib } from './ETH_like_compiled';
import type { EthLikeCompiled } from './ETH_like_interfaces';

export type { Token, PayAmt, AnyETH_Ty } from './ETH_like_compiled';
export * from './shared';

const ethCompiled: EthLikeCompiled = makeCompiledStdlib();
export const {
  // TODO: revisit which of these should actually be export
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
  stdlib,
  typeDefs,
} = ethCompiled;
