import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      getObj: () => {
        console.log('Alice getObj');
        return {y: true, x: 3}
      },
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasRandom,
      showObj: (obj, objHash, salt) => {
        console.log('Bob showObj');
        console.log({obj, objHash, salt});
      }
    }),
  ]);

  console.log('Hello, Alice and Bob!');
})();
