import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

  const stdlib = loadStdlib();
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.contract(backend);

  const checkView = async (x, expected) => {
    console.log('checkView', x, expected);
    assertEq(expected, await ctcAlice.v.Main.f(x)) };

  await Promise.all([
    backend.Alice(ctcAlice, { checkView }),
  ]);

