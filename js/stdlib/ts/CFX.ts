import * as cfxImpl from './CFX_impl';
import { makeEthLike } from './ETH_like';

export * from './CFX_compiled';
export const connector = 'CFX';

const ethLike = makeEthLike(cfxImpl);
// The following should be identical to ETH.ts
export const {
  getQueryLowerBound,
  setQueryLowerBound,
  getProvider,
  setProvider,
  randomUInt,
  hasRandom,
  setProviderByEnv,
  setProviderByName,
  providerEnvByName,
  getSignStrategy,
  setSignStrategy,
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
  newTestAccounts,
  getNetworkTime,
  waitUntilTime,
  wait,
  getNetworkSecs,
  waitUntilSecs,
  verifyContract,
  standardUnit,
  atomicUnit,
  parseCurrency,
  minimumBalance,
  formatCurrency,
  formatAddress,
  reachStdlib,
} = ethLike;
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = reachStdlib;
export * from './shared_user';
