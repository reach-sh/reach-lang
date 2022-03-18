import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {exps, acts});
  stdlib.assert(exps === acts) };
const startingBalance = stdlib.parseCurrency(100);
const [accAlice, accBob] = await stdlib.newTestAccounts(2, startingBalance);
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

const checkView = async (x, expected) => {
  console.log('checkView', x, expected);
  assertEq(expected, await ctcAlice.v.Main.f(x)) };

await Promise.all([
  backend.Alice(ctcAlice, { checkView }),
  backend.Bob(ctcBob, { checkView }),
]);
