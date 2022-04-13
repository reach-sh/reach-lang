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
  getTimeSecs,
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
