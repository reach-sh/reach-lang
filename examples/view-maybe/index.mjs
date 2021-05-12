import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);

  const checkView = async (expected) => {
    console.log('checkView', expected);
    assertEq(expected, await ctcAlice.getViews().Main.i()) };

  console.log(`It's starting`);

  await Promise.all([
    backend.Alice(ctcAlice, { checkView }),
  ]);

  console.log(`It's over`);
  await checkView(['None', null]);
})();
