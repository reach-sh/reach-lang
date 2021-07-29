import * as cfxers from './cfxers';
import * as ethLikeCompiled from './CFX_compiled';
import {
  debug,
  envDefault,
  memoizeThunk,
  replaceableThunk,
} from './shared_impl';
import { process, window } from './shim';
import waitPort from './waitPort';
import cfxsdk from 'js-conflux-sdk';
import Timeout from 'await-timeout';
const { Conflux } = cfxsdk;

type NetworkAccount = cfxers.IWallet; // XXX or other things
type Provider = cfxers.providers.Provider;

function notYetSupported(label: string): any {
  throw Error(`${label} not yet supported on CFX`);
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
  return true; // XXX
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

async function waitCaughtUp(provider: Provider): Promise<void> {
  await waitPort(CFX_NODE_URI);
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

const [getProvider, setProvider] = replaceableThunk<Promise<Provider>|Provider>(async (): Promise<Provider> => {
  // XXX parameterize impl on conflux
  const conflux = new Conflux({
    url: CFX_NODE_URI,
    // logger: console,
    networkId,
  });

  const provider = new cfxers.providers.Provider(conflux);

  // XXX is there a better place to wait for this
  // such that toying with things at the repl doesn't hang if no connection is available?
  await waitCaughtUp(provider);
  return provider;
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
