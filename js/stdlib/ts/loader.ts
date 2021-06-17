import * as stdlib_ETH from './ETH';
import * as stdlib_ALGO from './ALGO';
import * as stdlib_CFX from './CFX';
import {
  getConnectorMode,
  canonicalizeConnectorMode,
  getConnector
} from './ConnectorMode';
import {
  process,
  window
} from './shim';
import {
  rEnv,
  setDEBUG,
  truthyEnv,
} from './shared_impl';
import type { Stdlib_User } from './interfaces';

export { getConnectorMode, getConnector };

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export function loadStdlib(connectorModeOrEnv?: string | {[key: string]: string}): Stdlib_User<any> {
  if (!connectorModeOrEnv) {
    // @ts-ignore // XXX why doesn't TS understand that Env satisfies {[key: string}: string} ?
    return loadStdlib(process.env);
  }
  let connectorModeStr: string;
  if (typeof connectorModeOrEnv === 'string') {
    connectorModeStr = connectorModeOrEnv;
  } else if (connectorModeOrEnv['REACH_CONNECTOR_MODE']) {
    connectorModeStr = connectorModeOrEnv['REACH_CONNECTOR_MODE'];
  } else if (connectorModeOrEnv['REACT_APP_REACH_CONNECTOR_MODE']) {
    connectorModeStr = connectorModeOrEnv['REACT_APP_REACH_CONNECTOR_MODE'];
  } else {
    // TODO: also check {REACT_APP_,}REACH_DEFAULT_NETWORK
    connectorModeStr = 'ETH'; // If absolutely none specified/found, just default to 'ETH'
  }
  const connectorMode = canonicalizeConnectorMode(connectorModeStr);
  const connector = getConnector(connectorMode);
  let stdlib;
  switch (connector) {
    case 'ETH': stdlib = stdlib_ETH; break;
    case 'ALGO': stdlib = stdlib_ALGO; break;
    case 'CFX': stdlib = stdlib_CFX; break;
    default: throw Error(`impossible: unknown connector ${connector}`);
  }
  if (connectorModeOrEnv && typeof connectorModeOrEnv !== 'string') {
    let debug: boolean = truthyEnv(rEnv(connectorModeOrEnv, 'REACH_DEBUG'));
    setDEBUG(debug);
  }
  // also just inject ourselves into the window for ease of use
  window.reach = stdlib;
  return stdlib;
}
