import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);

  const decimals = 5;

  await Promise.all([
    backend.Alice(ctcAlice, {
      decimals,
      checkDecimals: async (tokId) => {
        const meta = await accAlice.tokenMetadata(tokId);
        console.assert(meta.decimals.toNumber() === decimals);
        console.log(`Token has ${decimals} decimals.`);
      }
    }),
  ]);
})();
