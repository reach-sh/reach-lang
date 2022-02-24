import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
import rightpad from 'rightpad';

const stdlib = loadStdlib();

const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(100));

const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

await Promise.all([
  backend.Alice(ctcAlice, {
    request: stdlib.parseCurrency(5),
    info: 'If you wear these, you can see beyond evil illusions.'
  }),
  backend.Bob(ctcBob, {
    want: (amt) => console.log(`Alice asked Bob for ${stdlib.formatCurrency(amt)}`),
    got: (secret) => console.log(`Alice's secret is: ${secret}`),
  }),
]);

// Note: rightpad is not in the default set of node deps.
// This is just a trivial example of using more node deps.
console.log(rightpad('123', 6) + '!')
