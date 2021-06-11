import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/%s.main.mjs';

(async () => {
  const stdlib = await loadStdlib(process.env);
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasRandom
    }),
  ]);

  console.log('Hello, Alice and Bob!');
})();
