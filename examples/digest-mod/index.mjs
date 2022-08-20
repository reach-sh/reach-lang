import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);
accAlice.setGasLimit(5000000);

const go = async (d) => {
  const ctcAlice = accAlice.contract(backend);

  const expected = stdlib.btoiLast8(d).mod(4);

  await Promise.all([
    backend.A(ctcAlice, {
      d,
      expected,
    }),
  ]);
};

const s = "ExampleeExampleeExampleeExamplee";
await go(stdlib.digest(stdlib.T_Bytes(32), s));
await go(stdlib.digest([stdlib.T_Bytes(32)], [s]));

