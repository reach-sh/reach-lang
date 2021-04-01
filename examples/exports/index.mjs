import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  console.log('Exports:');
  const exports = backend.getExports(stdlib);
  const a = exports.a;
  console.log(`a: ${JSON.stringify(a)}`);
  const o = exports.o;
  console.log(`o: ${JSON.stringify(o)}`);
  console.log(`call add1(1): ${exports.add1(1)}`);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasRandom
    }),
  ]);

})();
