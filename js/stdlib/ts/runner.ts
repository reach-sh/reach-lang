import { loadStdlib } from './loader';

const mountDir = process.env.REACH_RUNNER_MOUNT_DIR || '.';

// TODO: an interface for stdlib
type Stdlib = any;
type HasMain = {
  main: (stdlib: Stdlib, ...args: Array<String>) => void,
}

export async function run(file: string, ...args: Array<string>): Promise<void> {
  if (!file) { throw Error('Run requires an argument.'); }
  const m: HasMain = await import(`${mountDir}/${file}`);
  const stdlib = await loadStdlib();
  m.main(stdlib, ...args);
}

export function main(): void {
  // slice off $(which node) and runner file
  const [file, ...args] = process.argv.slice(2);
  run(file, ...args);
}

// TODO: only run main if running as script?
main();
