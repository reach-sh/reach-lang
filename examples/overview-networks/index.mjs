import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();

const whichNetwork = 0;

if (whichNetwork === 1)  {
  stdlib.setProviderByName('MainNet');
} else if (whichNetwork === 2) {
  stdlib.setProviderByName('TestNet');
} else if (whichNetwork === 3) {
  stdlib.setProviderByName('LocalHost');
}

const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(10000));
const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(10000));

const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

await Promise.all([
  ctcAlice.participants.Alice({
    request: stdlib.parseCurrency(5),
    info: 'If you wear these, you can see beyond evil illusions.'
  }),
  ctcBob.p.Bob({
    want: (amt) => console.log(`Alice asked Bob for ${stdlib.formatCurrency(amt)}`),
    got: (secret) => console.log(`Alice's secret is: ${secret}`),
  }),
]);
