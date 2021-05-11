import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const assertEq = (expected, actual) => {
    console.log('assertEq', {expected, actual});
    stdlib.assert(JSON.stringify(expected) === JSON.stringify(actual)) };
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const run = async (whosBob) => {
    const ctcAlice = accAlice.deploy(backend);
    const ctcBob = whosBob.attach(backend, ctcAlice.getInfo());

    const checkView = async (expected) => {
      console.log('checkView', expected);
      assertEq(expected, [
        await ctcAlice.getViews().Main.last(),
        await ctcAlice.getViews().Main.i(),
      ]) };

    await Promise.all([
      backend.Alice(ctcAlice, { checkView }),
      backend.Bob(ctcBob, {}),
    ]);
  };

  console.log(`Run w/ Bob`);
  await run(accBob);
  console.log(`Run w/ Alice`);
  await run(accAlice);
})();
