import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      {},
    ),
    backend.Bob(
      stdlib, ctcBob,
      {},
    ),
  ]);
})(); // <-- Don't forget these!
