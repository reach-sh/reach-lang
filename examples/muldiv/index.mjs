import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);
  console.log('Hello, Alice!');

  console.log('Launching...');
  const ctcAlice = accAlice.deploy(backend);

  console.log('Starting backends...');
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom,
      log: (msg, x) => console.log(msg, x.toString()),
      // This will be multiplied by 4 then divided by 5.
      // x * 4 would normally overflow on Algorand
      x: 9223372036854775807,
    }),
  ]);

  console.log('Goodbye, Alice!');
})();
