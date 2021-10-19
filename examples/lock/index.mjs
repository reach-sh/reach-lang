import {loadStdlib} from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const chi = await launchToken(stdlib, accAlice, "chi", "CHI");

  await accAlice.tokenAccept(chi.id);
  await chi.mint(accAlice, startingBalance);

  const ctcAlice = accAlice.contract(backend);

  await Promise.all([
    backend.Alice(ctcAlice, {
      cost: stdlib.parseCurrency(10),
      token: chi.id,
    }),
  ]);

})();
