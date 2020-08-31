import * as shared from './shared.mjs';

const stdlibFiles = {
  'ETH': './ETH.mjs',
  'ALGO': './ALGO.mjs',
  'FAKE': './FAKE.mjs',
};

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export function getConnector(connectorMode) {
  connectorMode = connectorMode || shared.getConnectorMode();
  return connectorMode.split('-')[0];
}

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export async function loadStdlib(connectorMode) {
  connectorMode = connectorMode ?
    shared.canonicalizeConnectorMode(connectorMode) :
    shared.getConnectorMode();
  const connector = getConnector(connectorMode);
  return await import(stdlibFiles[connector]);
}
