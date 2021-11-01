import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.deploy(backend);

  let lx = 0;
  await Promise.all([
    backend.Alice(ctcAlice, {
      x: () => {
        lx++;
        return lx;
      },
      view: async () => {
        const xv = await ctcAlice.v.xv();
        console.log(`xv: `, xv);
        const vxv = await ctcAlice.v.V.xv();
        console.log(`V.xv: `, vxv);
      }
    }),
  ]);
})();
