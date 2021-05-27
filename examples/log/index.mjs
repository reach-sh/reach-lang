import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);

  await Promise.all([
    backend.Alice(
      ctcAlice,
      { log: console.log },
    ),
  ]);
})();
