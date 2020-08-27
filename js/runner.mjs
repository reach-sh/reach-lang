const mountDir = process.env.REACH_RUNNER_MOUNT_DIR || '.';

const knownNetworks = ['ETH', 'ALGO', 'FAKE'];

async function getStdlib(network) {
  if (!knownNetworks.includes(network)) {
    throw Error(`Unknown network: ${network}`);
  }
  return await import(`./${network}.mjs`);
}

export async function run(network, file, ...args) {
  if (!file) { throw Error('Run requires an argument.'); }
  const stdlib = await getStdlib(network);
  const m = await import(`${mountDir}/${file}`);
  m.main(stdlib, ...args);
}

export function main() {
  // slice off $(which node) and runner file
  run(...process.argv.slice(2));
}

// TODO: only run main if running as script?
main();
