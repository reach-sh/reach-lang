import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import assert from 'assert';

  const stdlib = loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.contract(backend);
  const ctcBob = bob.contract(backend, ctcAlice.getInfo());

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
        assert(stdlib.addressEq(a, addrs['Alice']), "Did not receive Alice's address");
      },
    }),
  ]);
