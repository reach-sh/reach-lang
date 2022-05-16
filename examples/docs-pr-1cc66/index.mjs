import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
  const stdlib = loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const Alice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = Alice.contract(backend);
  await Promise.all([
    backend.Alice(ctcAlice, {
      pmt: stdlib.parseCurrency(0.01),
    }),
  ]);
