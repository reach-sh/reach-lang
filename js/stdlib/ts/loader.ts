import * as stdlib_ETH from './ETH';
import * as stdlib_ALGO from './ALGO';
import * as stdlib_FAKE from './FAKE';
import {getConnectorMode, canonicalizeConnectorMode, getConnector} from './ConnectorMode';
import {process} from './shim';

export {getConnectorMode, getConnector};

// XXX make an interface for Stdlib, return Promise<Stdlib>
// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export async function loadStdlib(connectorModeOrEnv?: string | {[key: string]: string}): Promise<any> {
  if (!connectorModeOrEnv) {
    // @ts-ignore // XXX why doesn't TS understand that Env satisfies {[key: string}: string} ?
    return await loadStdlib(process.env);
  }
  let connectorModeStr: string;
  if (typeof connectorModeOrEnv === 'string') {
    connectorModeStr = connectorModeOrEnv;
  } else if (connectorModeOrEnv['REACH_CONNECTOR_MODE']) {
    connectorModeStr = connectorModeOrEnv['REACH_CONNECTOR_MODE'];
  } else if (connectorModeOrEnv['REACT_APP_REACH_CONNECTOR_MODE']) {
    connectorModeStr = connectorModeOrEnv['REACT_APP_REACH_CONNECTOR_MODE'];
  } else {
    throw Error (`Argument to loadStdlib is missing a connector mode: ${loadStdlib}`);
  }
  const connectorMode = canonicalizeConnectorMode(connectorModeStr);
  const connector = getConnector(connectorMode);
  let stdlib;
  switch (connector) {
    case 'ETH': stdlib = stdlib_ETH; break;
    case 'ALGO': stdlib = stdlib_ALGO; break;
    case 'FAKE': stdlib = stdlib_FAKE; break;
    default: throw Error(`impossible: unknown connector ${connector}`);
  }
  if (connectorModeOrEnv && typeof connectorModeOrEnv !== 'string') {
    let debug: boolean = (connectorModeOrEnv['REACH_DEBUG'] || connectorModeOrEnv['REACT_APP_REACH_DEBUG']) ? true : false;
    stdlib.setDEBUG(debug);
  }
  return stdlib;
}
