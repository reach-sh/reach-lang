import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();

  const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(5));
  const accBob = await stdlib.newTestAccount(stdlib.parseCurrency(10));

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.A(ctcAlice, {
      getE: () => Math.floor(Math.random() * 3),
    }),
    backend.B(ctcBob, {
      showE: (e) => console.log(`Alice chose: ${e}`),
    }),
  ]);
})();
