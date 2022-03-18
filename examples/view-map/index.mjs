import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts) };
const startingBalance = stdlib.parseCurrency(100);
const [ accAlice, accBob ] = await Promise.all([
  stdlib.newTestAccount(startingBalance),
  stdlib.newTestAccount(startingBalance),
]);
accAlice.setDebugLabel('Alice');
accBob.setDebugLabel('Bob');
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

const checkView = async (x, who, fe, ge) => {
  console.log('checkView', x, who, stdlib.formatAddress(who), fe, ge);
  assertEq(fe, await ctcAlice.v.Main.f(who));
  assertEq(ge, await ctcAlice.v.Main.g(who));
};

await Promise.all([
  backend.Alice(ctcAlice, { checkView }),
  backend.Bob(ctcBob, { checkView }),
]);
