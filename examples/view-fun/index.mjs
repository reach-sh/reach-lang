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

  const checkView = async (x, expected) => {
    console.log('checkView', x, expected);
    assertEq(expected, await ctcAlice.getViews().Main.f(x)) };

  await Promise.all([
    backend.Alice(ctcAlice, { checkView }),
  ]);

})();
