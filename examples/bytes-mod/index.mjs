import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

accAlice.setGasLimit(5000000);

const ctcAlice = accAlice.contract(backend);

const b = "ExampleeExampleeExampleeExampleeExam";

await Promise.all([
  backend.A(ctcAlice, {
    b,
    expected: stdlib.btoiLast8(b).mod(4)
  }),
]);

