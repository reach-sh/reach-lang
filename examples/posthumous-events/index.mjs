import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice ] = await stdlib.newTestAccounts(1, startingBalance);
const ctc = accAlice.contract(backend);

// After this Promise finishes, the contract has exited
await backend.Alice(ctc, {});

// 'Posthumous' code, looking at events emitted while the contract was alive
const numMonPromise = 
  ctc.events.Numbers.number.monitor(ev => console.log(`Number event #${ev.what}`));

await Promise.race([numMonPromise, new Promise(r => setTimeout(r, 2000))]);
process.exit(0);