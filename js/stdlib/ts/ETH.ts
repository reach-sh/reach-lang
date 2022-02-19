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
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = reachStdlib;
export * from './shared_user';
