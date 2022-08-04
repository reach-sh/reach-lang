import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(startingBalance);
const ctcA = accA.contract(backend);

const Player = (acc) => ({
  getDT: () => {
    return ['A', null];
  },
  checkBool: (b) => {
    // Check result for test
    if (!b) {
      throw "isDataVariant test file got wrong answer"
    }
  }
})

await Promise.all([
  ctcA.p.A({
    ...Player(accA),
  }),
]);

