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
})

await Promise.all([
  ctcA.p.A({
    ...Player(accA),
  }),
]);

