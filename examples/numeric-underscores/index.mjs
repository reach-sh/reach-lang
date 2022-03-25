import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice ] = await stdlib.newTestAccounts(1, startingBalance);
const ctcAlice = accAlice.contract(backend);

await Promise.all([
  backend.Alice(ctcAlice, {
    verifyNumber: (n) => {
      if (n != 123_456_789) {
        throw Error(`didn't get the right number: ${n}`);
      }
    }
  }),
]);

