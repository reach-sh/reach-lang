import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = accAlice.deploy(backend);

  let howMany = 10;
  await Promise.all([
    backend.User(ctcAlice, {
      what: () => {
        if ( howMany-- < 0 ) { process.exit(0); }
        if ( Math.random() < 0.5 ) { 
          return [ 'Get', null ];
        } else {
          return [ 'Set', Math.floor(Math.random() * 10) ];
        }
      },
      get: (got) => {
        console.log(`Got ${got}`); },
      set: (val) => {
        console.log(`Set to ${val}`); },
    }),
  ]);

})();
