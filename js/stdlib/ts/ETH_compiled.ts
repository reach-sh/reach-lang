import { makeEthLikeCompiled } from './ETH_like_compiled';
import type { // =>
  EthLikeCompiled
} from './ETH_like_interfaces';
import * as ethCompiledImpl from './ETH_compiled_impl';

export type { // =>
  Token, PayAmt, AnyETH_Ty
} from './ETH_like_compiled';

const ethCompiled: EthLikeCompiled = makeEthLikeCompiled(ethCompiledImpl);
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
  band,
  bior,
  bxor,
  add256,
  sub256,
  mul256,
  div256,
  mod256,
  band256,
  bior256,
  bxor256,
  // end ...arith,

  // start ...typeDefs,
  T_Null,
  T_Bool,
  T_UInt,
  T_UInt256,
  T_Bytes,
  T_BytesDyn,
  T_StringDyn,
  T_Address,
  T_Contract,
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
