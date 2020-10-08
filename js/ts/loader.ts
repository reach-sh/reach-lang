import * as stdlib_ETH from './ETH';
import * as stdlib_ALGO from './ALGO';
import * as stdlib_FAKE from './FAKE';
import {getConnectorMode, canonicalizeConnectorMode, getConnector} from './ConnectorMode';

// XXX make an interface for Stdlib, return Promise<Stdlib>
// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export async function loadStdlib(connectorMode?: string): Promise<any> {
  connectorMode = connectorMode ?
    canonicalizeConnectorMode(connectorMode) :
    getConnectorMode();
  const connector = getConnector(connectorMode);
  switch (connector) {
    case 'ETH': return stdlib_ETH;
    case 'ALGO': return stdlib_ALGO;
    case 'FAKE': return stdlib_FAKE;
    default: throw Error(`impossible: unknown connector ${connector}`);
  }
  // const module = `./${connector}`;
  // try {
  //   return await import(module);
  // } catch (e) {
  //   try {
  //     return await import(`${module}.js`);
  //   } catch (e) {
  //     return await import(`${module}.mjs`);
  //   }
  // }
}
