import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const alice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = alice.deploy(backend);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      myAddr: (addr) => console.log(`I am: ${addr}`),
    }),
  ]);

  console.log('Alice is done.');
})();
