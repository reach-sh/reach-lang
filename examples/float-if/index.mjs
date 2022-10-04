import { Reach } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = new Reach();
const startingBal = stdlib.parseCurrency(1_000);
const [ aliceAcc ] = await stdlib.newTestAccounts(1, startingBal);

const ctcAlice = aliceAcc.contract(backend);

await Promise.all([
  backend.A(ctcAlice, {
    x: 2,
    y: 0,
    b: true,
    chk: (b) => {
      // should be true if x ==2
      console.assert(b == true);
    }
  })
]);
