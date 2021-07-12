import type { ethers } from 'ethers';
// Some simple shims for defining stuff across node & browser

type Process = {
  env: Env,
  stdout: Stdout,
}
type Env = {
  REACH_CONNECTOR_MODE?: string,
  REACH_DEBUG?: string,

  ETH_NODE_URI?: string,
  ETH_NET?: string,

  ALGO_FAUCET_PASSPHRASE?: string,
  ALGO_TOKEN?: string,
  ALGO_SERVER?: string,
  ALGO_PORT?: string,
  ALGO_INDEXER_TOKEN?: string,
  ALGO_INDEXER_SERVER?: string,
  ALGO_INDEXER_PORT?: string,

  CFX_NODE_URI?: string,
  CFX_NETWORK_ID?: string,
}
type Stdout = {
  write: (data: any) => void,
}
const processShim: Process = (() => {
  try {
    // XXX make better use of process-browserify
    if (Object.keys(process.env).length === 0) {
      throw Error(`nothing in process.env`);
    }
    return process;
  } catch (e) {
    // ReferenceError
    return {
      env: {
        // XXX: figure out how to handle this stuff better
        REACH_CONNECTOR_MODE: 'ETH-browser',
      },
      stdout: {
        write: () => {},
      },
    };
  }
})();

type Window = {
  ethereum?: ethers.providers.ExternalProvider,
  AlgoSigner?: any, // TODO
  conflux?: any, // TODO
  prompt?: (s: string) => string | null,
  reach?: any, // TODO
};

const windowShim: Window = (() => {
  try {
    // @ts-ignore
    return window;
  } catch (e) {
    // ReferenceError
    return {};
  }
})();

export {
  processShim as process,
  windowShim as window,
};
