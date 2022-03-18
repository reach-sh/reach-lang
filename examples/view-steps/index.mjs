import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts) };
const startingBalance = stdlib.parseCurrency(200);
const [ accA, accB ] = await stdlib.newTestAccounts(2, startingBalance);
accA.setDebugLabel('Alice');
accB.setDebugLabel('Bob');

const run = async (whosBob) => {
  const ctcA = accA.contract(backend);
  const ctcB = whosBob.contract(backend, ctcA.getInfo());

  const checkView = async (expected) => {
    console.log('checkView', expected);
    assertEq(expected, [
      await ctcA.v.Main.last(),
      await ctcA.v.Main.i(),
    ]) };

  await Promise.all([
    ctcA.p.Alice({ checkView }),
    ctcB.p.Bob({ checkView }),
  ]);
};

console.log(`Run w/ Bob`);
await run(accB);
console.log(`Run w/ Alice`);
await run(accA);
