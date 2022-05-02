import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
import { util } from '@reach-sh/stdlib';
const { thread } = util;

  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);
  await ctcAlice.p.Alice({
    deployed: async () => {
      const acc = await stdlib.newTestAccount(startingBalance);
      acc.setDebugLabel(`API`);
      const ctc = acc.contract(backend, ctcAlice.getInfo());
      const bob = ctc.a.Bob;
      const add1 = ctc.a.add1;

      const call = async (f, exp) => {
        let res = undefined;
        try {
          res = await f();
          console.assert(res.toNumber() == exp);
        } catch (e) {
          res = [`err`, e]
        }
        console.log(`res`, res.toNumber());
      };

      await call(() => add1(10), 11);
      await call(() => bob.add1(0), 1);
    },
  });
