import * as cfxers from './cfxers';
import * as ethLikeCompiled from './CFX_compiled';
import {
  debug,
  envDefault,
  memoizeThunk,
  replaceableThunk,
  truthyEnv,
} from './shared_impl';
import { process, window } from './shim';
import waitPort from './waitPort';
import cfxsdk from 'js-conflux-sdk';
import Timeout from 'await-timeout';
import { canonicalizeConnectorMode, ConnectorMode } from './ConnectorMode';
const { Conflux } = cfxsdk;

type NetworkAccount = cfxers.IWallet; // XXX or other things
type Provider = cfxers.providers.Provider;

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
  const env = getProviderEnv();
  return 'CFX_NET' in env && env.CFX_NET === 'window' && !!window.conflux;
}

// /**
//  * Strategies for deciding what getDefaultAccount returns.
//  */
// type SignStrategy
//   = 'secret'   // window.prompt for secret
//   | 'mnemonic' // window.prompt for mnemonic
//   | 'faucet'   // use the faucet account
//   | 'window'   // use window.conflux
//   | 'ConfluxPortal' // same as 'window'

export const [getSignStrategy, setSignStrategy] = replaceableThunk<string>(() => {
  // XXX make window.conflux the default at some point
  // if (window.conflux) {
  //   // XXX this should be more lenient about letting cp load later
  //   return 'window';
  // }

  if (window.prompt) {
    return 'secret';
  } else {
    // XXX this should only work on the devnet
    return 'faucet';
  }
});

export async function _getDefaultNetworkAccount(): Promise<NetworkAccount> {
  const provider = await getProvider();
  const promptFor = (s: string) => {
    if (!window.prompt) { throw Error(`Can't prompt user with window.prompt`); }
    return window.prompt(`Please paste your account's ${s}, or click cancel to generate a new one.`);
  }
  const ss = getSignStrategy();
  let w: cfxers.IWallet|null = null;
  switch (ss.toLowerCase()) {
  case 'secret':
    const skMay = promptFor('secret key');
    if (skMay) {
      const sk = skMay.slice(0, 2) == '0x' ? skMay : '0x' + skMay;
      w = new cfxers.Wallet(sk);
    } else {
      w = cfxers.Wallet.createRandom();
    }
    break;
  case 'mnemonic':
    const mnemonic = promptFor('mnemonic');
    w = mnemonic
      ? cfxers.Wallet.fromMnemonic(mnemonic)
      : cfxers.Wallet.createRandom();
    break;
  case 'window':
  case 'confluxportal':
    const cp = await getConfluxPortal();
    const addr = (await cp.enable())[0];
    w = new cfxers.BrowserWallet(cp, addr);
    break;
  case 'faucet':
    w = await _getDefaultFaucetNetworkAccount();
    break;
  default:
    throw Error(`Sign strategy not recognized: '${ss}'`);
  }
  if (!w) throw Error(`impossible: no account found for sign strategy '${ss}'`);
  if (!w.provider) w = w.connect(provider);
  return w;
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

async function waitCaughtUp(provider: Provider, env: ProviderEnv): Promise<void> {
  if ('CFX_NODE_URI' in env && env.CFX_NODE_URI && truthyEnv(env.REACH_DO_WAIT_PORT)) {
    await waitPort(env.CFX_NODE_URI);
  }
  if (isIsolatedNetwork()) {
    // XXX this doesn't work with setFaucet; requires the default faucet to be used
    // But we can't call getFaucet() or _getDefaultFaucetNetworkAccount() here because
    // those (if left to defaults) call getProvider which calls this fn (waitCaughtUp).
    // TODO: disentangle
    if (!defaultFaucetWallet.provider) defaultFaucetWallet.connect(provider);
    const maxTries = 20;
    const waitMs = 1000;
    let err: Error|null = null;
    for (let tries = 0; tries < maxTries; tries++) {
      if (err) {
        debug(`waitCaughtUp: waiting some more`, {waitMs, tries, maxTries, err});
        await Timeout.set(waitMs); // wait 1s between tries
      }
      try {
        const faddr = defaultFaucetWallet.getAddress();
        const fbal = await defaultFaucetWallet.provider?.conflux.getBalance(faddr);
        debug(`Faucet bal`, fbal);
        // @ts-ignore
        if (fbal == 0) {
          const failMsg = `Faucet balance is 0 (${faddr})`;
          debug(failMsg);
          throw Error(failMsg);
        }
        const w = cfxers.Wallet.createRandom().connect(provider);
        const txn = {to: w.getAddress(), value: '1'};
        debug(`sending dummy txn`, txn);
        const t = await defaultFaucetWallet.sendTransaction(txn);
        await t.wait();
        return;
      } catch (e) {
        // TODO: only loop again if we detect that it's the "not caught up yet" error
        //   err: RPCError: Request rejected due to still in the catch up mode.
        //   { code: -32077 }
        err = e;
      }
    }
    if (err) throw err;
  }
}

const [getProvider, _setProvider] = replaceableThunk<Promise<Provider>|Provider>(async (): Promise<Provider> => {
  const fullEnv = getProviderEnv();
  const provider = await waitProviderFromEnv(fullEnv);
  // XXX disentangle the places where we waitProvider vs waitCaughtUp

  // XXX is there a better place to wait for this
  // such that toying with things at the repl doesn't hang if no connection is available?
  await waitCaughtUp(provider, fullEnv);
  return provider;
});
export function setProvider(provider: Provider|Promise<Provider>): void {
  _setProvider(provider);
  if (!_providerEnv) {
    // this circumstance is weird and maybe we should handle it better
    // process.env isn't available in browser so we try to avoid relying on it here.
    setProviderEnv({
      // @ts-ignore
      CFX_NET: '__custom_unspecified__',
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
  CFX_NET: 'window'
  CFX_LOG: string
  REACH_CONNECTOR_MODE: string
  REACH_ISOLATED_NETWORK: string // preferably: 'yes' | 'no'
}

type ProviderByURI = {
  CFX_NODE_URI: string
  CFX_NETWORK_ID: string // XXX just query the URI for its net id?
  CFX_LOG: string
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

function guessConnectorMode(env: {[key: string]: string}): ConnectorMode|undefined {
  if ('CFX_NODE_URI' in env && env.CFX_NODE_URI) {
    // take a guess if CFX_NODE_URI is set
    return env.CFX_NODE_URI.toLowerCase().includes('localhost') ? 'CFX-devnet' : 'CFX-live';
  } else {
    // abstain from guessing
    return undefined;
  }
}

// XXX less copy/paste from ETH_impl
function envDefaultsCFX(env: {[key: string]: string}): ProviderEnv {
  const { CFX_NET, CFX_NODE_URI, CFX_NETWORK_ID } = env;
  const cm = envDefault(env.REACH_CONNECTOR_MODE, guessConnectorMode(env));
  const REACH_CONNECTOR_MODE = envDefault(cm, canonicalizeConnectorMode(env.REACH_CONNECTOR_MODE || 'CFX'));
  const isolatedDefault
    = connectorModeIsolatedNetwork(REACH_CONNECTOR_MODE);
    // XXX
    // CFX_NET === 'window' || window.conflux ? (windowLooksIsolated() ? 'yes' : 'no')
  const REACH_ISOLATED_NETWORK = envDefault(env.REACH_ISOLATED_NETWORK, isolatedDefault);
  const CFX_LOG = envDefault(env.CFX_LOG, 'no');
  if (truthyEnv(CFX_NET) && CFX_NET === 'window') {
    return { CFX_NET, CFX_LOG, REACH_CONNECTOR_MODE, REACH_ISOLATED_NETWORK };
  } else if (truthyEnv(CFX_NODE_URI)) {
    const REACH_DO_WAIT_PORT = envDefault(env.REACH_DO_WAIT_PORT, 'yes');
    return { CFX_NODE_URI, CFX_NETWORK_ID, CFX_LOG, REACH_CONNECTOR_MODE, REACH_DO_WAIT_PORT, REACH_ISOLATED_NETWORK };
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
    const {CFX_NODE_URI, CFX_NETWORK_ID, CFX_LOG, REACH_DO_WAIT_PORT} = env;
    return (async () => {
      if (truthyEnv(REACH_DO_WAIT_PORT)) await waitPort(CFX_NODE_URI);
      // await doHealthcheck(CFX_NODE_URI);
      // XXX ^ do health check?
      const networkId = CFX_NETWORK_ID ? parseInt(CFX_NETWORK_ID) : undefined;
      debug(`waitProviderFromEnv`, `new Conflux`, {url: CFX_NODE_URI, networkId});
      const provider = new cfxers.providers.Provider(new Conflux({
        url: CFX_NODE_URI,
        // XXX pass CFX_LOG around correctly; this isn't working
        logger: truthyEnv(CFX_LOG) ? console : undefined,
        networkId,
      }));
      // XXX: make some sort of configurable polling interval?
      // provider.pollingInterval = 500; // ms
      return provider;
    })();
  } else if ('CFX_NET' in env && env.CFX_NET) {
    const {CFX_NET} = env;
    if (CFX_NET === 'window') {
      const {conflux} = window;
      if (conflux) {
        return (async () => {
          return notYetSupported(`using window.conflux`);
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
    } else {
      throw Error(`CFX_NET not recognized: '${CFX_NET}'`);
    }
  } else {
    // This branch should be impossible, but just in case...
    throw Error(`non-empty CFX_NET or CFX_NODE_URI is required, got: ${Object.keys(env)}`);
  }
}

function setProviderByEnv(env: any): void {
  const fullEnv = envDefaultsCFX(env);
  setProviderEnv(fullEnv);
  setProvider(waitProviderFromEnv(fullEnv));
}

function setProviderByName(providerName: ProviderName): void {
  const env = providerEnvByName(providerName)
  setProviderByEnv(env);
}

const localhostProviderEnv: ProviderByURI = {
  CFX_NODE_URI: DEFAULT_CFX_NODE_URI,
  CFX_NETWORK_ID: DEFAULT_CFX_NETWORK_ID,
  CFX_LOG: 'no',
  REACH_CONNECTOR_MODE: 'CFX-devnet', // browser?
  REACH_DO_WAIT_PORT: 'yes',
  REACH_ISOLATED_NETWORK: 'yes',
}

function providerEnvByName(providerName: ProviderName): ProviderEnv {
  switch (providerName) {
  case 'LocalHost': return localhostProviderEnv;
  case 'window': return notYetSupported(`providerEnvByName('window')`);
  case 'MainNet': return providerEnvByName('tethys');
  case 'TestNet': return cfxProviderEnv('TestNet');
  case 'tethys': return cfxProviderEnv('tethys');
  case 'BlockNumber': return cfxProviderEnv('BlockNumber'); // XXX temporary
  default: throw Error(`Unrecognized provider name: ${providerName}`);
  }
}

function cfxProviderEnv(network: WhichNetExternal): ProviderByURI {
  const [CFX_NODE_URI, CFX_NETWORK_ID] =
      network == 'BlockNumber' ? ['http://52.53.235.44:12537', '1'] // 0x1 // XXX This isn't actually part of TestNet
    : network == 'TestNet' ? ['https://test.confluxrpc.com', '1'] // 0x1
    : network == 'tethys'  ? ['https://main.confluxrpc.com', '1029'] // 0x405
    : throwError(`network name not recognized: '${network}'`);
  return {
    CFX_NODE_URI,
    CFX_NETWORK_ID,
    CFX_LOG: 'no',
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


export { ethLikeCompiled };
export { cfxers as ethers };
export const providerLib = {
  getProvider,
  setProvider,
  setProviderByName,
  setProviderByEnv,
  providerEnvByName,
  getSignStrategy,
  setSignStrategy,
}
export const _warnTxNoBlockNumber = false; // XXX ?
export const standardUnit = 'CFX';
export const atomicUnit = 'Drip';
export const validQueryWindow = 1000;
