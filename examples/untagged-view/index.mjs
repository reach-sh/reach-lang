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
        const assert = (v) => {
          console.assert(v[1].toNumber() == lx - 1);
        }
        const xv = await ctcAlice.v.xv();
        assert(xv);
        const vxv = await ctcAlice.v.V.xv();
        assert(vxv);
      }
    }),
  ]);
})();
