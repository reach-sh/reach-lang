import { loadStdlib } from './loader.mjs';
const mountDir = process.env.REACH_RUNNER_MOUNT_DIR || '.';
export async function run(file, ...args) {
  if (!file) {
    throw Error('Run requires an argument.');
  }
  const m = await import(`${mountDir}/${file}`);
  const stdlib = await loadStdlib();
  m.main(stdlib, ...args);
}
export function main() {
  // slice off $(which node) and runner file
  const [file, ...args] = process.argv.slice(2);
  run(file, ...args);
}
// TODO: only run main if running as script?
main();
