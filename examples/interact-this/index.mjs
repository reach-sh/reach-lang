import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import assert from 'assert';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  const addrs = {
    'Alice': alice.getAddress(),
    'Bob': bob.getAddress(),
  };
  console.log(`Alice address: ${addrs['Alice']}`);
  console.log(`Bob address: ${addrs['Bob']}`);

  await Promise.all([
    backend.Alice(ctcAlice, {}),
    backend.Bob(ctcBob, {
      show: (a) => {
        console.log(`Bob sees Alice's address`, a);
        assert(a == addrs['Alice'], "Did not receive Alice's address");
      },
    }),
  ]);
})();
