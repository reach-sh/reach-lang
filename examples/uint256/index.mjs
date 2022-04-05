import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts); };
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const ctcA = accA.contract(backend);
const answers = [
];
await ctcA.p.A({
  vs1: [],
  check: (i, rsh) => assertEq(answers[i], rsh),
});
