import { makeCompiledStdlib } from './ETH_like_compiled';

export type { Token, PayAmt, AnyETH_Ty } from './ETH_like_compiled';

export const {
  T_UInt,
  digest,
  addressEq,
  tokenEq,
  typeDefs,
  stdlib,
} = makeCompiledStdlib();
