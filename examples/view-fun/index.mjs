import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const assertEq = (x, y) =>
    stdlib.assert(JSON.stringify(x) === JSON.stringify(y));
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());
  const views = ctcAlice.getViews();

  await Promise.all([
    backend.Alice(ctcAlice, {
    }),
    backend.Bob(ctcBob, {
    }),
  ]);
})();
