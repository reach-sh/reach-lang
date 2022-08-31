import {loadStdlib} from '@reach-sh/stdlib';
import * as backendC from './build/index.mainC.mjs';
import * as backendS from './build/index.mainS.mjs';

const stdlib = loadStdlib();

const sBal = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, sBal);

const ctcA = accA.contract(backendS);
console.log('A starting');
await ctcA.p.D({
  ready: async (serverInfo) => {
    console.log('A ready with', serverInfo);
    const ctcB = accB.contract(backendC);
    console.log('B starting with', serverInfo);
    await ctcB.p.D({ serverInfo,
      log: async (...args) => {
        console.log('B saw', ...args);
      }
    });
    console.log('B done');
  },
});
console.log('A done');
