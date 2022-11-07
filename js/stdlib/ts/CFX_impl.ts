import * as cfxers from './cfxers';
import * as ethLikeCompiled from './CFX_compiled';
import {
  debug,
  envDefault,
  memoizeThunk,
  replaceableThunk,
  truthyEnv,
} from './shared_impl';
import {
  bigNumberify,
} from './shared_user';
import type { Env } from './shim'; // =>
import { process, window } from './shim';
import waitPort from './waitPort';
import cfxsdk from '@reach-sh/js-conflux-sdk';
import Timeout from 'await-timeout';
import { canonicalizeConnectorMode, ConnectorMode } from './ConnectorMode';
import buffer from 'buffer';
const { Buffer } = buffer;
const { Conflux } = cfxsdk;

type NetworkAccount = cfxers.IWallet; // XXX or other things
export type Provider = cfxers.providers.Provider;

function notYetSupported(label: string): any {
  throw Error(`${label} not yet supported on CFX`);
}

function throwError(msg: string): any {
  throw Error(msg);
}

const DEFAULT_CFX_NODE_URI = 'http://localhost:12537';
const DEFAULT_CFX_NETWORK_ID = '999';

export function isIsolatedNetwork(): boolean {
  return truthyEnv(getProviderEnv().REACH_ISOLATED_NETWORK);
}

export function isWindowProvider(): boolean {
  return !!window.conflux;
}

export function canGetDefaultAccount(): boolean {
  // XXX be pickier
  return true;
}

export async function _getDefaultNetworkAccount(): Promise<NetworkAccount> {
  const cp = await getConfluxPortal();
  // @ts-ignore
  const w = cp._rwfb ? cp._rwfb()
    : new cfxers.BrowserWallet(cp, (await cp.enable())[0]);
  if (w.provider) {
    return w;
  } else {
    return w.connect(await getProvider());
  }
}

// from /scripts/devnet-cfx/default.toml
const mining_key = '0x091ca0785ec2bd9a5eca245fdc83baddd570644f3e0489b41e515f0e5c33f3d9';
const defaultFaucetWallet = new cfxers.Wallet(mining_key);

export const _getDefaultFaucetNetworkAccount = memoizeThunk(async (): Promise<NetworkAccount> => {
  if (!defaultFaucetWallet.provider) {
    const provider = await getProvider();
    // Async things can cause this state to change...
    if (!defaultFaucetWallet.provider) defaultFaucetWallet.connect(provider);
  }
  return defaultFaucetWallet;
});

function toHexAddr(cfxAddr: string) {
  return '0x' + Buffer.from(
    // @ts-ignore
    cfxsdk.address.decodeCfxAddress(cfxAddr).hexAddress
  ).toString('hex').toLowerCase();
}

const makeURLFunder = (url: string) => async (to:any, amt:any): Promise<void> => {
  const dhead = 'doURLFunder';
  to = to.getAddress ? await to.getAddress() : to;
  debug(dhead, to);
  const toHex = toHexAddr(to);
  let u = `${url}?address=${toHex}`;
  if ( amt ) { u = `${u}&amount=${bigNumberify(amt)}`; }
  debug(dhead, { toHex, u });
  const res = await window.fetch(u);
  const resJson = await res.json();
  debug(dhead, { resJson });
  if ( ! res.ok ) { throw resJson; }
};

export async function canFundFromFaucet() {
  debug('canFundFromFaucet');
  const netId = ethLikeCompiled.getNetworkId();
  return netId == 0x1 || netId == 999;
}

export async function _specialFundFromFaucet() {
  const ni = ethLikeCompiled.getNetworkId();
  debug(`_specialFundFromFaucet`, {ni});
  if (ni == 0x1) {
    // XXX TestNet faucet only gives out 100 CFX at a time
    // Should we throw an error if amt !== 100 CFX?
    return makeURLFunder(`http://test-faucet.confluxnetwork.org:18088/dev/ask`);
  } else if (ni == 999) {
    const env = getProviderEnv();
    const k = 'CFX_NODE_URI';
    const base = k in env ? env[k] : DEFAULT_CFX_NODE_URI;
    const coms = base.split(':');
    coms.pop();
    const uri = coms.join(':');
    return makeURLFunder(`${uri}:1337/faucet`);
  } else {
    return null;
  }
}

const [getProvider, _setProvider] = replaceableThunk<Promise<Provider>>(async (): Promise<Provider> => {
  const env = getProviderEnv();
  const provider = await waitProviderFromEnv(env);
  if ('CFX_NODE_URI' in env && env.CFX_NODE_URI && truthyEnv(env.REACH_DO_WAIT_PORT)) {
    await waitPort(env.CFX_NODE_URI);
  }
  return provider;
});
export function setProvider(provider: Promise<Provider>): void {
  _setProvider(provider);
  if (!_providerEnv) {
    // this circumstance is weird and maybe we should handle it better
    // process.env isn't available in browser so we try to avoid relying on it here.
    setProviderEnv({
      REACH_CONNECTOR_MODE: 'CFX-unspecified',
      REACH_ISOLATED_NETWORK: 'no',
    });
  }
};

export type WhichNetExternal
  = 'tethys'
  | 'TestNet' // XXX name?
  | 'BlockNumber' // XXX not permanent

// TODO: more providers 'by name'
export type ProviderName
  = WhichNetExternal
  | 'MainNet'
  | 'TestNet'
  | 'LocalHost'
  | 'window'

export interface ProviderByWindow {
  REACH_CONNECTOR_MODE: string
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}

type ProviderByURI = {
  CFX_NODE_URI: string
  CFX_NETWORK_ID: string // XXX just query the URI for its net id?
  REACH_CONNECTOR_MODE: string
  REACH_DO_WAIT_PORT: string // preferably: 'yes' | 'no'
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}
export type ProviderEnv = ProviderByURI | ProviderByWindow

function connectorModeIsolatedNetwork(cm: string): 'yes' | 'no' {
  switch (cm) {
  case 'CFX-devnet': return 'yes';
  default: return 'no';
  }
}

function guessConnectorMode(env: Env): ConnectorMode|undefined {
  if ('CFX_NODE_URI' in env && env.CFX_NODE_URI) {
    // take a guess if CFX_NODE_URI is set
    return env.CFX_NODE_URI.toLowerCase().includes('localhost') ? 'CFX-devnet' : 'CFX-live';
  } else {
    // abstain from guessing
    return undefined;
  }
}

// XXX less copy/paste from ETH_impl
function envDefaultsCFX(env: Env): ProviderEnv {
  const { CFX_NODE_URI, CFX_NETWORK_ID } = env;
  const cm = envDefault(env.REACH_CONNECTOR_MODE, guessConnectorMode(env));
  const REACH_CONNECTOR_MODE = envDefault(cm, canonicalizeConnectorMode(env.REACH_CONNECTOR_MODE || 'CFX'));
  const isolatedDefault
    = connectorModeIsolatedNetwork(REACH_CONNECTOR_MODE);
  const REACH_ISOLATED_NETWORK = envDefault(env.REACH_ISOLATED_NETWORK, isolatedDefault);
  if (truthyEnv(CFX_NODE_URI)) {
    const REACH_DO_WAIT_PORT = envDefault(env.REACH_DO_WAIT_PORT, 'yes');
    const cni = envDefault(CFX_NETWORK_ID, localhostProviderEnv.CFX_NETWORK_ID);
    return { CFX_NODE_URI, CFX_NETWORK_ID: cni, REACH_CONNECTOR_MODE, REACH_DO_WAIT_PORT, REACH_ISOLATED_NETWORK };
  } else {
    if (window.conflux) {
      return localhostProviderEnv;
      // XXX instead of this ^ support using window.conflux as provider
      // return notYetSupported(`window.conflux`);
      // return windowProviderEnv(REACH_ISOLATED_NETWORK);
    } else {
      return localhostProviderEnv;
    }
  }
}

// Avoid using _providerEnv directly; use get/set
// We don't use replaceableThunk because slightly more nuanced inspection needs to be possible.
let _providerEnv: ProviderEnv|undefined;
function getProviderEnv(): ProviderEnv {
  if (!_providerEnv) {
    // We only fall back on process.env if there no setProviderEnv occurrs
    const env = envDefaultsCFX(process.env);
    _providerEnv = env;
  }
  return _providerEnv;
}
function setProviderEnv(env: ProviderEnv): void {
  if (_providerEnv) {
    throw Error(`setProviderEnv called after it was already set`);
  }
  _providerEnv = env;
  if ('CFX_NETWORK_ID' in env) {
    try {
      const networkId = parseInt(env.CFX_NETWORK_ID);
      ethLikeCompiled.setNetworkId(networkId);
    } catch (_) {
      throw Error(`Invalid CFX_NETWORK_ID='${env.CFX_NETWORK_ID}'`);
    }
  }
}

// XXX less copy/pasta from ETH_impl
async function waitProviderFromEnv(env: ProviderEnv): Promise<Provider> {
  if ('CFX_NODE_URI' in env && env.CFX_NODE_URI) {
    const {CFX_NODE_URI, CFX_NETWORK_ID, REACH_DO_WAIT_PORT} = env;
    return (async () => {
      if (truthyEnv(REACH_DO_WAIT_PORT)) await waitPort(CFX_NODE_URI);
      // await doHealthcheck(CFX_NODE_URI);
      // XXX ^ do health check?
      const networkId = CFX_NETWORK_ID ? parseInt(CFX_NETWORK_ID) : undefined;
      debug(`waitProviderFromEnv`, `new Conflux`, {url: CFX_NODE_URI, networkId});
      const provider = new cfxers.providers.Provider(new Conflux({
        url: CFX_NODE_URI,
        networkId,
        // Uncomment if you want CFX logs on every API call
        // logger: console,
      }));
      // XXX: make some sort of configurable polling interval?
      // provider.pollingInterval = 500; // ms
      return provider;
    })();
  } else {
    const {conflux} = window;
    if (conflux) {
      return (async () => {
        return notYetSupported(`using window.conflux as provider.`);
        // TODO
        // const provider = new ethers.providers.Web3Provider(ethereum);
        // // The proper way to ask MetaMask to enable itself is eth_requestAccounts
        // // https://eips.ethereum.org/EIPS/eip-1102
        // await provider.send('eth_requestAccounts', []);
        // return provider;
      })();
    } else {
      throw Error(`window.conflux is not defined`);
    }
  }
}

function setProviderByEnv(env: any): void {
  const fullEnv = envDefaultsCFX(env);
  setProviderEnv(fullEnv);
  setProvider(waitProviderFromEnv(fullEnv));
}

function setProviderByName(pn: ProviderName): void {
  const env = providerEnvByName(pn)
  setProviderByEnv(env);
}

const localhostProviderEnv: ProviderByURI = {
  CFX_NODE_URI: DEFAULT_CFX_NODE_URI,
  CFX_NETWORK_ID: DEFAULT_CFX_NETWORK_ID,
  REACH_CONNECTOR_MODE: 'CFX-devnet', // browser?
  REACH_DO_WAIT_PORT: 'yes',
  REACH_ISOLATED_NETWORK: 'yes',
}

function providerEnvByName(pn: ProviderName): ProviderEnv {
  switch (pn) {
  case 'LocalHost': return localhostProviderEnv;
  case 'window': return notYetSupported(`providerEnvByName('window')`);
  case 'MainNet': return providerEnvByName('tethys');
  case 'TestNet': return cfxProviderEnv('TestNet');
  case 'tethys': return cfxProviderEnv('tethys');
  case 'BlockNumber': return cfxProviderEnv('BlockNumber'); // XXX temporary
  default: throw Error(`Unrecognized provider name: ${pn}`);
  }
}

function cfxProviderEnv(network: WhichNetExternal): ProviderByURI {
  const [CFX_NODE_URI, CFX_NETWORK_ID] =
      network == 'BlockNumber' ? ['http://52.53.235.44:12537', '1'] // 0x1
    : network == 'TestNet' ? ['https://portal-test.confluxrpc.com', '1'] // 0x1
    : network == 'tethys'  ? ['https://portal-main.confluxrpc.com', '1029'] // 0x405
    : throwError(`network name not recognized: '${network}'`);
  return {
    CFX_NODE_URI,
    CFX_NETWORK_ID,
    REACH_DO_WAIT_PORT: 'yes',
    REACH_CONNECTOR_MODE: 'CFX-live',
    REACH_ISOLATED_NETWORK: 'no',
  }
}

async function getConfluxPortal(): Promise<cfxers.CP> {
  const maxTries = 10;
  for (let tries = 1; tries <= maxTries; tries++) {
    if (window.conflux) return window.conflux;
    await Timeout.set(100);
  }
  throw Error(`Couldn't find window.conflux`);
}

const setWalletFallback = (wf:() => any) => {
  if ( ! window.conflux ) { window.conflux = wf(); }
};
const walletFallback = (opts:any) => () => {
  // XXX do cfx provider from opts
  const p = {};
  if (opts?.providerEnv) {
    throw Error(`providerEnv not supported in this context`); // yet
  }
  // TODO: reduce duplication with ETH_impl
  // @ts-ignore
  p._rwfb = () => {
    const mnem = window.prompt(`Please paste the mnemonic for your account, or enable ConfluxPortal and refresh the page.`);
    return mnem ? cfxers.Wallet.fromMnemonic(mnem) : cfxers.Wallet.createRandom();
  };
  return p;
};

export { ethLikeCompiled };
export { cfxers as ethers };
export const providerLib = {
  getProvider,
  setProvider,
  setProviderByName,
  setProviderByEnv,
  providerEnvByName,
  setWalletFallback,
  walletFallback,
}
export const _warnTxNoBlockNumber = false; // XXX ?
export const standardUnit = 'CFX';
export const atomicUnit = 'Drip';

// This can probably be 999, but Dan is superstitious,
// and wants to avoid off-by-ones by a larger margin
export const validQueryWindow = 990;
