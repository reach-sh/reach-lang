import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(100);

const accA = await stdlib.newTestAccount(startingBalance);
const ctcA = accA.contract(backend);

await ctcA.p.A({
  id: (x) => {
    console.log(`THIS IS X:`, x);
    return x;
  },
});
