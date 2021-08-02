import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/${APP}.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      // implement Alice's interact object here
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasRandom,
      // implement Bob's interact object here
    }),
  ]);

  console.log('Hello, Alice and Bob!');
})();
