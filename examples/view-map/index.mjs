import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  if ( stdlib.connector === 'ALGO' ) {
    console.log(`XXX Unsupported`);
    process.exit(0);
  }
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const checkView = async (x, expected) => {
    console.log('checkView', x, expected);
    assertEq(expected, await ctcAlice.getViews().Main.f(x)) };

  await Promise.all([
    backend.Alice(ctcAlice, { checkView }),
    backend.Bob(ctcBob, {}),
  ]);

})();
