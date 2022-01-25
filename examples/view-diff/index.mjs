import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = await loadStdlib();
const bal = stdlib.parseCurrency(100);
const accG = await stdlib.newTestAccount(bal);
const ctcG = accG.contract(backend);
await ctcG.p.G({
  ready: async () => {
    await ctcG.unsafeViews.V.t();
    process.exit(0);
  },
});
