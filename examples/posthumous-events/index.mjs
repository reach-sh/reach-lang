import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice, accObserver ] = await stdlib.newTestAccounts(2, startingBalance);
const ctcAlice = accAlice.contract(backend);
const ctcObserver = accObserver.contract(backend, ctcAlice.getInfo());

// After this Promise finishes, the contract has exited
await Promise.all([
  ctcAlice.p.Alice({}),
  ctcObserver.p.Observer({}),
]);

// 'Posthumous' code, looking at events emitted while the contract was alive
const ctcEvents = accObserver.contract(backend, ctcAlice.getInfo());
for (var n = 0; n < 10; n++) {
  const ev = await ctcEvents.events.Numbers.number.next();
  console.log(`Number event #${ev.what}`);
}
