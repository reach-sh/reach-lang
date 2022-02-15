import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);
  const tok1 = await stdlib.launchToken(accAlice, 'ZMD', 'ZMD');
  const tok2 = await stdlib.launchToken(accAlice, 'GIL', 'GIL');

  const getBals = async () => [
    await stdlib.balanceOf(accAlice, tok1.id),
    await stdlib.balanceOf(accAlice, tok2.id),
  ]

  await accAlice.tokenAccept(tok1.id);
  await accAlice.tokenAccept(tok2.id);
  await tok1.mint(accAlice, startingBalance);
  await tok2.mint(accAlice, startingBalance);

  const ogBals = await getBals();
  const amt = 100000;

  await Promise.all([
    backend.A(ctcAlice, {
      tok1: tok1.id,
      tok2: tok2.id,
      cond: true,
      amt,
      log: console.log,
      checkBal: async (i) => {
        const actBals = await getBals();
        const expBals = {
          0: ogBals,
          1: [ ogBals[0].sub(amt), ogBals[1] ],
          2: ogBals }[i];
        stdlib.assert(JSON.stringify(actBals), JSON.stringify(expBals));
      }
    }),
  ]);
})();
