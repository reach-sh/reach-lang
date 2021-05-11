import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const assertEq = (expected, actual) => {
    console.log('assertEq', {expected, actual});
    stdlib.assert(JSON.stringify(expected) === JSON.stringify(actual)) };
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);

  const go = async (expected) => {
    console.log('go', expected);
    assertEq(expected, await ctcAlice.getViews().Main.i()) };

  console.log(`It's starting`);

  await Promise.all([
    backend.Alice(ctcAlice, { go }),
  ]);

  console.log(`It's over`);
  await go(['None', null]);
})();
