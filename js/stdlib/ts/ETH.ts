import { makeEthLike } from './ETH_like'
import * as ethImpl from './ETH_impl';

// TODO: export some types from ./shared?

const ethLike = makeEthLike(ethImpl);

export const {
  // start ...shared,
  getViewsHelper,
  deferContract,
  truthyEnv,
  envDefault,
  setDEBUG,
  getDEBUG,
  debug,
  assert,
  isBigNumber,
  checkedBigNumberify,
  protect,
  isHex,
  hexToString,
  stringToHex,
  makeDigest,
  hexToBigNumber,
  uintToBytes,
  bigNumberToHex,
  bytesEq,
  digestEq,
  makeRandom,
  eq,
  makeArith,
  ge,
  gt,
  le,
  lt,
  argsSlice,
  argsSplit,
  Array_set,
  Array_zip,
  mapRef,
  objectMap,
  mkAddressEq,
  parseFixedPoint,
  parseInt,
  // end ...shared,

  // start ...arith,
  add,
  sub,
  mod,
  mul,
  div,
  // end ...arith,

  // start ...typeDefs
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
  // end ...typeDefs

  // start EthLikeBackendStdlib (+ above)
  addressEq,
  tokenEq,
  digest,
  UInt_max,
  // end EthLikeBackendStdlib

  // start EthLikeCompiled (+ above)
  stdlib,
  typeDefs,
  // end EthLikeCompiled

  // start EthLike (+ above)
  getProvider,
  setProvider,
  randomUInt,
  hasRandom,
  setProviderByEnv,
  setProviderByName,
  providerEnvByName,
  balanceOf,
  transfer,
  connectAccount,
  newAccountFromSecret,
  newAccountFromMnemonic,
  getDefaultAccount,
  getFaucet,
  setFaucet,
  createAccount,
  fundFromFaucet,
  newTestAccount,
  getNetworkTime,
  wait,
  waitUntilTime,
  verifyContract,
  standardUnit,
  atomicUnit,
  parseCurrency,
  minimumBalance,
  formatCurrency,
  formatAddress,
  reachStdlib,
  // end EthLike

} = ethLike;
