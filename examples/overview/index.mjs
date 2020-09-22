import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(5));
  const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(10));

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  await Promise.all([
    backend.Alice(stdlib, ctcAlice, {
      request: stdlib.parseCurrency(5),
      info: 'If you wear these, you can see beyond evil illusions.'
    }),
    backend.Bob(stdlib, ctcBob, {
      want: (amt) => console.log(`Alice asked Bob for ${stdlib.formatCurrency(amt)}`),
      got: (secret) => console.log(`Alice's secret is: ${stdlib.hexToString(secret)}`),
    }),
  ]);
})();
