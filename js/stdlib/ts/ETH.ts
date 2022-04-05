import { makeEthLike } from './ETH_like'
import * as ethImpl from './ETH_impl';
export type { Provider, ProviderEnv, ProviderName } from './ETH_impl';
export type { Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Account } from './ETH_like';

export * from './ETH_compiled';
export const connector = 'ETH';
export * as ethers from 'ethers';

const ethLike = makeEthLike(ethImpl);
// The following should be identical to CFX.ts
export const {
  doCall,
  getQueryLowerBound,
  setQueryLowerBound,
  getValidQueryWindow,
  setValidQueryWindow,
  getProvider,
  setProvider,
  randomUInt,
  hasRandom,
  setProviderByEnv,
  setProviderByName,
  providerEnvByName,
  setWalletFallback,
  walletFallback,
  balanceOf,
  balancesOf,
  minimumBalanceOf,
  transfer,
  connectAccount,
  newAccountFromSecret,
  newAccountFromMnemonic,
  getDefaultAccount,
  getFaucet,
  setFaucet,
  createAccount,
  canFundFromFaucet,
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
  formatWithDecimals,
  unsafeGetMnemonic,
  launchToken,
  reachStdlib,
  setMinMillisBetweenRequests,
  setCustomHttpEventHandler,
  setSigningMonitor,
} = ethLike;
export const {
  add, sub, mod, mul, div, band, bior, bxor, eq, ge, gt, le, lt,
  add256, sub256, mod256, mul256, div256, band256, bior256, bxor256, eq256, ge256, gt256, le256, lt256,
  cast, muldiv,
  protect, assert, Array_set,
  bytesEq, digestEq, digest_xor, bytes_xor, btoiLast8
} = reachStdlib;
export * from './shared_user';
