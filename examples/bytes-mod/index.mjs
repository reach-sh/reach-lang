import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

accAlice.setGasLimit(5000000);

const ctcAlice = accAlice.contract(backend);

const b4 = "ExampleeExampleeExampleeExampleeExamplee";
const b1 = b4.slice(0, 5);  // "Examp";
const b2 = b4.slice(0, 24); // "ExampleeExampleeExamplee";
const b3 = b4.slice(0, 36); // "ExampleeExampleeExampleeExampleeExam";

await Promise.all([
  backend.A(ctcAlice, {
    b1, b2, b3, b4,
    expected1: stdlib.btoiLast8(b1).mod(4),
    expected2: stdlib.btoiLast8(b2).mod(4),
    expected3: stdlib.btoiLast8(b3).mod(4),
    expected4: stdlib.btoiLast8(b4).mod(4),
  }),
]);

