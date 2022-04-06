import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const bn = stdlib.bigNumberify;
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts); };
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(100));
accA.setGasLimit(5000000);
const ctcA = accA.contract(backend);
const b1 = bn(2).pow(128);
const b2 = bn(2).pow(63);
const s1 = bn(2).pow(48);
const s2 = bn(2).pow(32);
const answers = [
];
await ctcA.p.A({
  vs1: [ b1, b2, s1, s2 ],
  check: (i, rsh) => assertEq(answers[i], rsh),
});
