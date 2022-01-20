import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);

const ctcA = accA.contract(backend);

await ctcA.p.A({
  who1: accA,
  who2: accB,
});
