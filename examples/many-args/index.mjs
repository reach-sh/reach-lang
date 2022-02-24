import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

  const stdlib = loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.contract(backend);

  const look = (...args) => {
    console.log(args); };
  await Promise.all([
    backend.Alice(ctcAlice, { ...stdlib.hasRandom, look }),
  ]);

