import { ethers } from 'ethers';
import url from 'url';
import http from 'http';
import { canonicalizeConnectorMode } from './ConnectorMode';
import * as ethLikeCompiled from './ETH_compiled';
import {
  debug,
  envDefault,
  getDEBUG,
  truthyEnv,
  replaceableThunk
} from './shared_impl';
import { process, window } from './shim';
import waitPort from './waitPort';

type Provider = ethers.providers.Provider;
type NetworkAccount = any // XXX

// We don't ever do waitPort for these so it's not an option.
// The reason this level of indirection still exists here
// is because ethers.js does a cool thing by default:
// It queries multiple providers.
export interface ProviderByName {
  ETH_NET: string // WhichNetExternal | 'window'
  REACH_CONNECTOR_MODE: string
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}

export type WhichNetExternal
  = 'homestead'
  | 'ropsten'

// TODO: more providers 'by name'
export type ProviderName
  = WhichNetExternal
  | 'MainNet'
  | 'TestNet'
  | 'LocalHost'
  | 'window'

// TODO: support config of custom api keys
export interface ProviderByURI {
  ETH_NODE_URI: string
  REACH_CONNECTOR_MODE: string
  REACH_DO_WAIT_PORT: string // preferably: 'yes' | 'no'
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}

export type ProviderEnv = ProviderByURI | ProviderByName


export {ethLikeCompiled};

// TODO: types on these
export async function _getDefaultNetworkAccount(): Promise<NetworkAccount> {
  debug(`_getDefaultAccount`);
  const provider = await getProvider();
  // @ts-ignore
  const signer = provider.getSigner();
  return signer;
}

// TODO: types on these
export async function _getDefaultFaucetNetworkAccount(): Promise<NetworkAccount> {
  if (isIsolatedNetwork()) {
    if (isWindowProvider()) {
      // XXX only localhost:8545 is supported
      const p = new ethers.providers.JsonRpcProvider('http://localhost:8545');
      return p.getSigner();
    }
    // XXX this may break if users call setProvider?
    // On isolated networks, the default account is assumed to be the faucet.
    // Furthermore, it is assumed that the faucet Signer is "unlocked",
    // so no further secrets need be provided in order to access its funds.
    // This is true of reach-provided devnets.
    // TODO: allow the user to set the faucet via mnemnonic.
    return await _getDefaultNetworkAccount();
  }
  throw Error(`getFaucet not supported in this context.`)
}

// Not an async fn because it throws some errors synchronously, rather than in the Promise thread
function waitProviderFromEnv(env: ProviderEnv): Promise<Provider> {
  if ('ETH_NODE_URI' in env && env.ETH_NODE_URI) {
    const {ETH_NODE_URI, REACH_DO_WAIT_PORT} = env;
    return (async () => {
      if (truthyEnv(REACH_DO_WAIT_PORT)) await waitPort(ETH_NODE_URI);
      await doHealthcheck(ETH_NODE_URI);
      const provider = new ethers.providers.JsonRpcProvider(ETH_NODE_URI);
      // TODO: make polling interval configurable?
      provider.pollingInterval = 500; // ms
      return provider;
    })();
  } else if ('ETH_NET' in env && env.ETH_NET) {
    const {ETH_NET} = env;
    // TODO: support more
    if (ETH_NET === 'homestead' || ETH_NET === 'ropsten') {
      // No waitPort for these, just go
      return Promise.resolve(ethers.getDefaultProvider(ETH_NET));
    } else if (ETH_NET === 'window') {
      const {ethereum} = window;
      if (ethereum) {
        return (async () => {
          const provider = new ethers.providers.Web3Provider(ethereum);
          // The proper way to ask MetaMask to enable itself is eth_requestAccounts
          // https://eips.ethereum.org/EIPS/eip-1102
          await provider.send('eth_requestAccounts', []);
          return provider;
        })();
      } else {
        throw Error(`window.ethereum is not defined`);
      }
    } else {
      throw Error(`ETH_NET not recognized: '${ETH_NET}'`);
    }
  } else {
    // This branch should be impossible, but just in case...
    throw Error(`non-empty ETH_NET or ETH_NODE_URI is required, got: ${Object.keys(env)}`);
  }
}

function setProviderByEnv(env: Partial<ProviderByName & ProviderByURI>): void {
  const fullEnv = envDefaultsETH(env);
  setProviderEnv(fullEnv);
  setProvider(waitProviderFromEnv(fullEnv));
}

export function setProviderByName(providerName: ProviderName): void  {
  const env = providerEnvByName(providerName);
  setProviderByEnv(env);
}

const localhostProviderEnv: ProviderByURI = {
  ETH_NODE_URI: 'http://localhost:8545',
  REACH_CONNECTOR_MODE: 'ETH-test-dockerized-geth',
  REACH_DO_WAIT_PORT: 'yes',
  REACH_ISOLATED_NETWORK: 'yes',
}

function windowProviderEnv(REACH_ISOLATED_NETWORK: string = 'no'): ProviderByName {
  return {
    ETH_NET: 'window',
    REACH_CONNECTOR_MODE: 'ETH-browser',
    REACH_ISOLATED_NETWORK,
  }
}

function ethersProviderEnv(network: WhichNetExternal): ProviderByName {
  return {
    ETH_NET: network,
    REACH_CONNECTOR_MODE: 'ETH-live',
    REACH_ISOLATED_NETWORK: 'no',
  }
}

function providerEnvByName(providerName: ProviderName): ProviderEnv {
  switch (providerName) {
  case 'LocalHost': return localhostProviderEnv;
  case 'window': return windowProviderEnv();
  case 'MainNet': return providerEnvByName('homestead');
  case 'TestNet': return providerEnvByName('ropsten');
  case 'homestead': return ethersProviderEnv('homestead');
  case 'ropsten': return ethersProviderEnv('ropsten');
  default: throw Error(`Unrecognized provider name: ${providerName}`);
  }
}

// Avoid using _providerEnv directly; use get/set
// We don't use replaceableThunk because slightly more nuanced inspection needs to be possible.
let _providerEnv: ProviderEnv|undefined;
function getProviderEnv(): ProviderEnv {
  if (!_providerEnv) {
    // We only fall back on process.env if there no setProviderEnv occurrs
    const env = envDefaultsETH(process.env);
    _providerEnv = env;
  }
  return _providerEnv;
}
function setProviderEnv(env: ProviderEnv): void {
  if (_providerEnv) {
    throw Error(`setProviderEnv called after it was already set`);
  }
  _providerEnv = env;
}

export function isIsolatedNetwork(): boolean {
  return truthyEnv(getProviderEnv().REACH_ISOLATED_NETWORK);
}

export function isWindowProvider(): boolean {
  const env = getProviderEnv();
  return 'ETH_NET' in env && env.ETH_NET === 'window' && !!window.ethereum;
}

function windowLooksIsolated() {
  if (!window.ethereum) return false;
  // XXX this is a hacky way of checking if we're on a devnet
  // @ts-ignore // 0x539 = 1337
  return (window.ethereum.chainId === '0xNaN' || window.ethereum.chainId == '0x539');
}

function connectorModeIsolatedNetwork(connectorMode: string): 'yes' | 'no' {
  switch (connectorMode) {
  case 'ETH-test-dockerized-geth': return 'yes';
  default: return 'no';
  }
}

function guessConnectorMode(env: Partial<ProviderByName & ProviderByURI>): string|undefined {
  if ('ETH_NODE_URI' in env && env.ETH_NODE_URI) {
    // take a guess if ETH_NODE_URI is set
    return env.ETH_NODE_URI.toLowerCase().includes('localhost') ? 'ETH-test-dockerized-geth' : 'ETH-live';
  } else {
    // abstain from guessing
    return undefined;
  }
}

function envDefaultsETH(env: Partial<ProviderByName & ProviderByURI>): ProviderEnv {
  const { ETH_NET, ETH_NODE_URI } = env;
  const cm = envDefault(env.REACH_CONNECTOR_MODE, guessConnectorMode(env));
  const REACH_CONNECTOR_MODE = envDefault(cm, canonicalizeConnectorMode(env.REACH_CONNECTOR_MODE || 'ETH'));
  const isolatedDefault
    = ETH_NET && ETH_NET !== 'window' ? 'no'
    : ETH_NET === 'window' || window.ethereum ? (windowLooksIsolated() ? 'yes' : 'no')
    : connectorModeIsolatedNetwork(REACH_CONNECTOR_MODE);
  const REACH_ISOLATED_NETWORK = envDefault(env.REACH_ISOLATED_NETWORK, isolatedDefault);
  if (truthyEnv(ETH_NET)) {
    return { ETH_NET, REACH_CONNECTOR_MODE, REACH_ISOLATED_NETWORK };
  } else if (truthyEnv(ETH_NODE_URI)) {
    const REACH_DO_WAIT_PORT = envDefault(env.REACH_DO_WAIT_PORT, 'no');
    return { ETH_NODE_URI, REACH_CONNECTOR_MODE, REACH_DO_WAIT_PORT, REACH_ISOLATED_NETWORK };
  } else {
    if (window.ethereum) {
      return windowProviderEnv(REACH_ISOLATED_NETWORK);
    } else {
      const { REACH_DO_WAIT_PORT } = env;
      if (truthyEnv(REACH_DO_WAIT_PORT)) {
        return {...localhostProviderEnv, REACH_DO_WAIT_PORT};
      } else {
        return localhostProviderEnv;
      }
    }
  }
}

const [getProvider, _setProvider] = replaceableThunk<Promise<Provider>|Provider>(async (): Promise<Provider> => {
  const fullEnv = getProviderEnv();
  return await waitProviderFromEnv(fullEnv);
});
export {getProvider};
export function setProvider(provider: Provider|Promise<Provider>): void {
  // TODO: define ETHProvider to be {provider: Provider, isolated: boolean} ?
  // Maybe also {window: boolean}
  _setProvider(provider);
  if (!_providerEnv) {
    // this circumstance is weird and maybe we should handle it better
    // process.env isn't available in browser so we try to avoid relying on it here.
    setProviderEnv({
      ETH_NET: '__custom_unspecified__',
      REACH_CONNECTOR_MODE: 'ETH-unspecified',
      REACH_ISOLATED_NETWORK: 'no',
    });
  }
};

// XXX: doesn't even retry, just returns the first attempt
const doHealthcheck = async (theUrl: string): Promise<void> => {
  debug('doHealthcheck');
  const urlObj = url && url.parse && url.parse(theUrl);

  // XXX the code below only supports http
  if (!urlObj || urlObj.protocol !== 'http:') { return; }

  await new Promise((resolve, reject) => {
    const data = JSON.stringify({
      jsonrpc: '2.0',
      method: 'web3_clientVersion',
      params: [],
      id: 67,
    });
    debug('Sending health check request...');
    const opts = {
      ...urlObj,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': data.length,
      },
    };
    const req = http.request(opts, (res) => {
      debug(`statusCode:`, res.statusCode);
      res.on('data', (d) => {
        debug('rpc health check succeeded');
        if (getDEBUG()) {
          process.stdout.write(d);
        }
        resolve({ res, d });
      });
    });
    req.on('error', (e) => {
      console.log('rpc health check failed');
      console.log(e);
      reject(e);
    });
    req.write(data);
    debug('attached all the handlers...');
    req.end();
    debug('req.end...');
  });
};

function getSignStrategy(): string {
  throw Error(`getSignStrategy not yet implemented on ETH`);
}
function setSignStrategy(ss: string) {
  void(ss);
  throw Error(`setSignStrategy not yet implemented on ETH`);
}

export { ethers };
export const providerLib = {
  getProvider,
  setProvider,
  setProviderByName,
  setProviderByEnv,
  setSignStrategy,
  getSignStrategy,
  providerEnvByName,
}
export const standardUnit = 'ETH';
export const atomicUnit = 'WEI';
