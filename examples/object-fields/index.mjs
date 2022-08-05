import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(startingBalance);
const ctcA = accA.contract(backend);

const Player = (acc) => ({
  seeObj: ((o) => {
    if (! Object.keys(o).length == 3) {
      throw "got bad object"
    }
  }),
  seeStruct: ((o) => {
    if (! Object.keys(o).length == 3) {
      throw "got bad object"
    }
  }),
})

await Promise.all([
  ctcA.p.A({
    ...Player(accA),
  }),
]);
