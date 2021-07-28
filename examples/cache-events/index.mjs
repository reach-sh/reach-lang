import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);

  let i = 0;
  await Promise.all([ backend.A(ctcAlice, {
    get: () => {
      i += 1;
      return i;
    },
    spawn: () => {
      const ctcBob = accBob.attach(backend, ctcAlice.getInfo());
      Promise.all([ backend.B(ctcBob, {}) ]);
    }
  }) ]);

  console.log(`Done`);
  process.exit();

})();
