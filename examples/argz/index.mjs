import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

console.log(process.argv);
stdlib.assert(process.argv[3] === 'Mr. Postman');

(async () => {
  const startingBalance = stdlib.parseCurrency({ETH: 100});

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = await alice.deploy(backend);
  const ctcBob = await bob.attach(backend, ctcAlice);

  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      { ...stdlib.hasRandom }
    ),
    backend.Bob(
      stdlib, ctcBob,
      { ...stdlib.hasRandom }
    ),
  ]);

  console.log('Hello, Alice and Bob!');
})();
