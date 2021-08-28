import {
  setDEBUG,
  truthyEnv,
} from './shared_impl';
import node_fetch from 'node-fetch';
// Some simple shims for defining stuff across node & browser

type Process = {
  env: Env,
  stdout: Stdout,
}
export type Env = {[key: string]: string|undefined};
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
      _reachShim: true,
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

export const updateProcessEnv = (x:Env): void => {
  const env = processShim.env;
  for ( const k in x ) {
    const kp = k.replace(/^REACT_APP_/,"");
    env[kp] = x[k];
  }
  setDEBUG(truthyEnv(env['REACH_DEBUG']));
};

type Window = {[key: string]: any};
const windowShim: Window = (() => {
  try {
    // @ts-ignore
    return window;
  } catch (e) {
    // ReferenceError
    return {
      _reachShim: true,
      fetch: node_fetch,
    };
  }
})();

export {
  processShim as process,
  windowShim as window,
};
