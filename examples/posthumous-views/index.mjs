import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [accAlice, accBob] = await stdlib.newTestAccounts(2, startingBalance);
const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

// After this Promise finishes, the contract has exited
await Promise.all([
  backend.Alice(ctcAlice, {
    observeNumber: async () => {
      const maybeN = await ctcAlice.views.Number.number();
      console.log(`Alices observes the current number as ${maybeN[1]}`);
    },
  }),
  backend.Bob(ctcBob, {}),
]);

// 'Posthumous' code, looking at view set while the contract was alive
const maybeN = await ctcAlice.views.Number.number();
if (maybeN[0] !== 'None') {
  throw Error('view set after ctc exited');
}

console.log('After the ctc exited, the current number is None');
