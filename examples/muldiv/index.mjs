import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  console.log(`Deploying...`);
  const ctcAlice = accAlice.deploy(backend);

  const x = stdlib.bigNumberify(9223372036854775807);
  const y = stdlib.bigNumberify(4);
  const z = stdlib.bigNumberify(5);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      check: (actual) => {
        const expected = x.mul(y).div(z);
        console.log(`Check: `, actual.toString(), `=`, expected.toString());
        console.assert(actual.eq(expected));
      },
      // x will be multiplied by y then divided by z.
      // x * y would normally overflow on Algorand
      x, y, z,
    }),
  ]);

  console.log('Done!');
})();
