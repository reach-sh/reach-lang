import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);
  const ctcAlice = accAlice.contract(backend);

  const interact = {
    log: console.log,
    t: 5,
  };

  await Promise.all([
    backend.Alice(ctcAlice, interact),
  ]);

})();
