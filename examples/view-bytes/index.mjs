import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts) };
const startingBalance = stdlib.parseCurrency(100);
const [accAlice, accBob] = await stdlib.newTestAccounts(2, startingBalance);
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

const checkView = async (expected) => {
  console.log('checkView', expected);
  assertEq(expected, [
    await ctcBob.v.Main.who(),
    await ctcBob.v.Main.meta(),
  ])};

const meta = `This is a test string`;
await Promise.all([
  backend.Alice(ctcAlice, { meta }),
  backend.Bob(ctcBob, { checkView }),
]);

