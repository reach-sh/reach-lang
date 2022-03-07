import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice, accObserver ] = await stdlib.newTestAccounts(2, startingBalance);
const ctcAlice = accAlice.contract(backend);

// After this Promise finishes, the contract has exited
await backend.Alice(ctcAlice, {});

// 'Posthumous' code, looking at events emitted while the contract was alive
const ctcObserver = accObserver.contract(backend, ctcAlice.getInfo());

for (var n = 0; n < 10; n++) {
  const ev = await ctcObserver.events.Numbers.number.next();
  console.log(`Number event #${ev.what}`);
}
