import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib(process.env);

  const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(100));
  const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(100));

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
})();
