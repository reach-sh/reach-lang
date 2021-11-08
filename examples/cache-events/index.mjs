import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10 * 25);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  accAlice.setDebugLabel('Alice');
  accBob.setDebugLabel('Bob');

  const ctcAlice = accAlice.deploy(backend);

  let i = 0;
  await Promise.all([ backend.A(ctcAlice, {
    get: () => {
      i += 1;
      console.log(`Alice iterates: ${i}`);
      return i;
    },
    spawn: () => {
      console.log(`Alice spawns Bob`);
      const ctcBob = accBob.attach(backend, ctcAlice.getInfo());
      Promise.all([ backend.B(ctcBob, {}) ]);
    }
  }) ]);

  console.log(`Done`);
  process.exit();

})();
