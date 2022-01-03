import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const alice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = alice.contract(backend);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      myAddr: (addr) => console.log(`I am: ${stdlib.formatAddress(addr)}`),
    }),
  ]);

  console.log('Alice is done.');
})();
