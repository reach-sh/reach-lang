import ethers from 'ethers';
// Some simple shims for defining stuff across node & browser

type Process = {
  env: Env,
  stdout: Stdout,
}
type Env = {
  REACH_CONNECTOR_MODE?: string,
  ETH_NODE_URI?: string,
  ETH_NODE_NETWORK?: string,
}
type Stdout = {
  write: (data: any) => void,
}
const processShim: Process = (() => {
  try {
    return process;
  } catch (e) {
    // ReferenceError
    return {
      env: {
        // XXX: figure out how to handle this stuff better 
        REACH_CONNECTOR_MODE: 'ETH-test-browser'
      },
      stdout: {
        write: () => {},
      },
    }
  }
})();

type Window = {ethereum?: ethers.providers.ExternalProvider};

const windowShim: Window = (() => {
  try {
    // @ts-ignore
    return window;
  } catch (e) {
    // ReferenceError
    return {}
  }
})();

export {
  processShim as process,
  windowShim as window,
}
