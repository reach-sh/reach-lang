import * as stdlib_ETH from './ETH';
import * as stdlib_ALGO from './ALGO';
import {
  getConnectorMode,
  canonicalizeConnectorMode,
  getConnector
} from './ConnectorMode';
import type { Env } from './shim'; // =>
import {
  process,
  window,
  updateProcessEnv,
} from './shim';
import type { Stdlib_User } from './interfaces'; // =>
import { doStdlibLoad } from './registry';

export { unsafeAllowMultipleStdlibs } from './registry';
export { getConnectorMode, getConnector };

function extractMode(x?: string | Env): string {
  if ( ! x ) {
    return extractMode(process.env);
  }
  if ( typeof x === 'string' ) {
    return extractMode({REACH_CONNECTOR_MODE: x});
  }
  updateProcessEnv(x);
  const g = process.env['REACH_CONNECTOR_MODE'];
  if ( ! g ) {
    console.log(`WARNING: \`REACH_CONNECTOR_MODE\` defaulting behavior is deprecated as of`
              + ` version 0.1.6; please update your code to set this value explicitly.`);
    return 'ETH';
  } else {
    return g;
  }
};

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export function loadStdlib(connectorModeOrEnv?: string | {[key: string]: string}): Stdlib_User<any, any, any, any, any, any, any, any, any, any, any> {
  const connectorModeStr = extractMode(connectorModeOrEnv);
  const connectorMode = canonicalizeConnectorMode(connectorModeStr);
  const connector = getConnector(connectorMode);

  // Remember the connector to prevent users from accidentally using multiple stdlibs
  doStdlibLoad(connector);

  let stdlib_x;
  switch (connector) {
    case 'ETH': stdlib_x = stdlib_ETH; break;
    case 'ALGO': stdlib_x = stdlib_ALGO; break;
    default: throw Error(`impossible: unknown connector ${connector}`);
  }

  const stdlib = stdlib_x.load();
  // also just inject ourselves into the window for ease of use
  window.reach = stdlib;
  return stdlib;
}

export function Reach(this: {}, connectorModeOrEnv?: string | {[key: string]: string}) {
  Object.assign(this, loadStdlib(connectorModeOrEnv));
}
