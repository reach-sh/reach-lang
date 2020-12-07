import stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

console.log(process.argv);
stdlib.assert(process.argv[3] === 'Mr. Postman');

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.Alice(
      ctcAlice,
      { ...stdlib.hasRandom }
    ),
    backend.Bob(
      ctcBob,
      { ...stdlib.hasRandom }
    ),
  ]);

  console.log('Hello, Alice and Bob!');
})();
