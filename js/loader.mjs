// Fragment defaulting goes to to bottom
const knownConnectorModes = [
  'ETH-test-geth-dockerized',
  'ETH-test-ganache-embedded',
  'FAKE-test-mock-embedded',
  'ALGO-test-something-dockerized',
];

const connectorModeDefaults = {
};

// Populate connectorModeDefaults
for (const knownConnectorMode of knownConnectorModes) {
  let prefix = false;
  for (const piece of knownConnectorMode.split('-')) {
    prefix = prefix ? `${prefix}-${piece}` : piece;
    if (!connectorModeDefaults[prefix]) {
      connectorModeDefaults[prefix] = knownConnectorMode;
    }
  }
}

export function canonicalizeConnectorMode(connectorMode) {
  const canonicalized = connectorModeDefaults[connectorMode];
  if (typeof canonicalized === 'string') {
    return canonicalized;
  } else {
    throw Error(`Unrecognized REACH_CONNECTOR_MODE=${connectorMode}`);
  }
}

export function getConnectorMode() {
  const connectorMode = process.env.REACH_CONNECTOR_MODE || 'ETH';
  return canonicalizeConnectorMode(connectorMode);
}

const stdlibFiles = {
  'ETH': './ETH.mjs',
  'ALGO': './ALGO.mjs',
  'FAKE': './FAKE.mjs',
};

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export function getConnector(connectorMode) {
  connectorMode = connectorMode || getConnectorMode();
  return connectorMode.split('-')[0];
}

// The connectorMode arg is optional;
// It will use REACH_CONNECTOR_MODE if 0 args.
export async function loadStdlib(connectorMode) {
  connectorMode = connectorMode ?
    canonicalizeConnectorMode(connectorMode) :
    getConnectorMode();
  const connector = getConnector(connectorMode);
  return await import(stdlibFiles[connector]);
}
