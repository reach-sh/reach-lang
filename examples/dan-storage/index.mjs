import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);
  const accO = await stdlib.newTestAccount(startingBalance);
  const ctcO = accO.contract(backend);

  await ctcO.p.Oracle({
    ...stdlib.hasConsoleLogger,
    i: 5,
    getWord: (dialog) => {
      console.log(`getWord`, dialog);
      return '';
    },
  });
})();
