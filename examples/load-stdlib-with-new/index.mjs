import { Reach, loadStdlib } from '@reach-sh/stdlib';

const stdlib_loadStdlib = loadStdlib(process.env);
const stdlib_newReach = new Reach(process.env);

const keys_loadStdlib = Object.keys(stdlib_loadStdlib);
const keys_newReach = Object.keys(stdlib_newReach);

if (!keys_loadStdlib.every((key, idx) => key === keys_newReach[idx])) {
  throw "They're supposed to be the same";
}
