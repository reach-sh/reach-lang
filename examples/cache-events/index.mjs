import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(10 * 25);

const accAlice = await stdlib.newTestAccount(startingBalance);
const accBob = await stdlib.newTestAccount(startingBalance);
accAlice.setDebugLabel('Alice');
accBob.setDebugLabel('Bob');

const ctcAlice = accAlice.contract(backend);

let i = 0;
await Promise.all([ backend.A(ctcAlice, {
  get: () => {
    i += 1;
    console.log(`Alice iterates: ${i}`);
    return i;
  },
  spawn: () => {
    console.log(`Alice spawns Bob`);
    const ctcBob = accBob.contract(backend, ctcAlice.getInfo());
    Promise.all([ backend.B(ctcBob, {}) ]);
  }
}) ]);
