import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);

  const look = (...args) => {
    console.log(args); };
  await Promise.all([
    backend.Alice(ctcAlice, { ...stdlib.hasRandom, look }),
  ]);

})();
