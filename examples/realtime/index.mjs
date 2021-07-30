import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);
  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);
  console.log(accAlice);
  const ctcAlice = accAlice.deploy(backend);

  const interact = {
    log: console.log,
    t: 5,
  };

  await Promise.all([
    backend.Alice(ctcAlice, interact),
  ]);

})();
