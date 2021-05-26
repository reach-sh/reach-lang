import * as cfxers from './cfxers';
import * as ethLikeCompiled from './CFX_compiled';
import { memoizeThunk, replaceableThunk } from './shared_impl';
import { envDefault } from './shared';
import { process } from './shim';
import waitPort from './waitPort';

import cfxsdk from 'js-conflux-sdk';
const { Conflux } = cfxsdk;

type NetworkAccount = cfxers.Wallet; // XXX or other things
type Provider = cfxers.providers.Provider;

function notYetSupported(label: string): any {
  throw Error(`${label} not yet supported on experimental connector CFX`);
}

// XXX incorporate these into setProviderByEnv
const DEFAULT_CFX_NODE_URI = 'http://localhost:12537';
const DEFAULT_CFX_NETWORK_ID = '999';
const CFX_NODE_URI = envDefault(process.env.CFX_NODE_URI, DEFAULT_CFX_NODE_URI);
const CFX_NETWORK_ID = envDefault(process.env.CFX_NETWORK_ID, DEFAULT_CFX_NETWORK_ID);
const networkId = parseInt(CFX_NETWORK_ID);

export function isIsolatedNetwork(): boolean {
  return true; // XXX
}

export function isWindowProvider(): boolean {
  return false; // XXX
}

export async function _getDefaultNetworkAccount(): Promise<NetworkAccount> {
  return notYetSupported(`_getDefaultNetworkAccount`);
}

export const _getDefaultFaucetNetworkAccount = memoizeThunk(async (): Promise<NetworkAccount> => {
  // from /scripts/devnet-cfx/default.toml
  const mining_key = "0xaa911f5b5b567af4db867a9d9072f4415fe722b114306baae28b721b6fbb2d99"
  return (new cfxers.Wallet(mining_key)).connect(await getProvider());
});

const [getProvider, setProvider] = replaceableThunk<Promise<Provider>|Provider>(async (): Promise<Provider> => {
  await waitPort(CFX_NODE_URI);

  // XXX parameterize impl on conflux
  const conflux = new Conflux({
    url: CFX_NODE_URI,
    // logger: console,
    networkId,
  });

  // XXX We need to find a way to wait until catch-up mode is done

  return new cfxers.providers.Provider(conflux);
});

function setProviderByEnv(env: any): void {
  void(env);
  return notYetSupported(`setProviderByEnv`);
}

function setProviderByName(providerName: any): void {
  void(providerName);
  return notYetSupported(`setProviderByName`);
}

function providerEnvByName(providerName: any): void {
  void(providerName);
  return notYetSupported(`providerEnvByName`);
}

export { ethLikeCompiled };
export { cfxers as ethers };
export const providerLib = {
  getProvider,
  setProvider,
  setProviderByName,
  setProviderByEnv,
  providerEnvByName,
}
export const _verifyContractCode = false; // XXX
export const _warnTxNoBlockNumber = false; // XXX ?
export const standardUnit = 'CFX';
export const atomicUnit = 'Drip';
