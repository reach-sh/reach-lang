import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
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
  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  const checkView = async (x, who, expected) => {
    console.log('checkView', x, who, stdlib.formatAddress(who), expected);
    assertEq(expected, await ctcAlice.v.Main.f(who)) };

  await Promise.all([
    backend.Alice(ctcAlice, { checkView }),
    backend.Bob(ctcBob, {}),
  ]);

})();
