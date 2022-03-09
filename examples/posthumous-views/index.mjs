import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice ] = await stdlib.newTestAccounts(1, startingBalance);
const ctc = accAlice.contract(backend);

// After this Promise finishes, the contract has exited
await backend.Alice(ctc, {
  observeNumber: async () => {
    const maybeN = await ctc.views.Number.number();
    console.log(`Alice observes the current number as ${maybeN[1]}`);
  },
});

// 'Posthumous' code, looking at view set while the contract was alive
const maybeN = await ctc.views.Number.number();
if (maybeN[0] !== 'None') {
  throw Error("view set after ctc exited");
}

console.log("After the ctc exited, the current number is None")
