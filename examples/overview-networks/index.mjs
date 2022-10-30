import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();

const whichNetwork = 0;

if (whichNetwork === 1)  {
  stdlib.setProviderByName('MainNet'); // Default AlgoNode MainNet
} else if (whichNetwork === 2) {
  stdlib.setProviderByName('TestNet'); // Default AlgoNode TestNet
} else if (whichNetwork === 3) {
  stdlib.setProviderByName('BetaNet'); // Default AlgoNode BetaNet
} else if (whichNetwork === 4) {
  stdlib.setProviderByName('algonode/MainNet'); // AlgoNode MainNet
} else if (whichNetwork === 5) {
  stdlib.setProviderByName('algonode/TestNet'); // AlgoNode TestNet
} else if (whichNetwork === 6) {
  stdlib.setProviderByName('algonode/BetaNet'); // AlgoNode BetaNet
} else if (whichNetwork === 7) {
  stdlib.setProviderByName('randlabs/MainNet'); // RandLabs MainNet
} else if (whichNetwork === 8) {
  stdlib.setProviderByName('randlabs/TestNet'); // RandLabs TestNet
} else if (whichNetwork === 9) {
  stdlib.setProviderByName('randlabs/BetaNet'); // RandLabs BetaNet
} else if (whichNetwork === 10) {
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
