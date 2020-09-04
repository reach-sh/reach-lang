import { getStdlib } from './loader.mjs';

const mountDir = process.env.REACH_RUNNER_MOUNT_DIR || '.';

export async function run(file, ...args) {
  if (!file) { throw Error('Run requires an argument.'); }
  const m = await import(`${mountDir}/${file}`);
  const stdlib = await getStdlib();
  m.main(stdlib, ...args);
}

export function main() {
  // slice off $(which node) and runner file
  run(...process.argv.slice(2));
}

// TODO: only run main if running as script?
main();
