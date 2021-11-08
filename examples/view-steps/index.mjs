import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const now = await stdlib.getNetworkTime();
  stdlib.setQueryLowerBound(now);
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(200);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const run = async (whosBob) => {
    const ctcAlice = accAlice.contract(backend);
    const ctcBob = whosBob.contract(backend, ctcAlice.getInfo());

    const checkView = async (expected) => {
      console.log('checkView', expected);
      assertEq(expected, [
        await ctcAlice.v.Main.last(),
        await ctcAlice.v.Main.i(),
      ]) };

    await Promise.all([
      ctcAlice.p.Alice({ checkView }),
      ctcBob.p.Bob({}),
    ]);
  };

  console.log(`Run w/ Bob`);
  await run(accBob);
  console.log(`Run w/ Alice`);
  await run(accAlice);
})();
