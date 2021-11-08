import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib(process.env);

  const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(100));
  const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(100));

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

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
})();
