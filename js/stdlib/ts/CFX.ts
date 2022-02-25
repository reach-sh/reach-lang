import * as cfxImpl from './CFX_impl';
export type { Provider, ProviderEnv, ProviderName } from './CFX_impl';
import { makeEthLike } from './ETH_like';
export type { Token, ContractInfo, Address, NetworkAccount, Ty, Backend, Account } from './ETH_like';

export * as ethers from './cfxers';
export * from './CFX_compiled';
export const connector = 'CFX';

const ethLike = makeEthLike(cfxImpl);
// The following should be identical to ETH.ts
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
export const { add, sub, mod, mul, div, protect, assert, Array_set, eq, ge, gt, le, lt, bytesEq, digestEq } = reachStdlib;
export * from './shared_user';
