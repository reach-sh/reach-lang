import { makeEthLikeCompiled } from './ETH_like_compiled';
import { EthLikeCompiled } from './ETH_like_interfaces';
import * as cfxCompiledImpl from './CFX_compiled_impl';

const cfxCompiled: EthLikeCompiled = makeEthLikeCompiled(cfxCompiledImpl);
export const { setNetworkId } = cfxCompiledImpl;
// The following should be identical to ETH_compiled.ts
export const {
  stdlib,
  typeDefs,
} = cfxCompiled;
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
