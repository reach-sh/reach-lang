import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
import { util } from '@reach-sh/stdlib';
const { thread } = util;

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);

const ctcAlice = accAlice.contract(backend);
await ctcAlice.p.Alice({
  deployed: async () => {
    const uid = 'Bob';
    const acc = await stdlib.newTestAccount(startingBalance);
    acc.setDebugLabel(uid);
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const bob = ctc.a.Bob;

    const call = async (f) => {
      let res = undefined;
      try {
        res = await f();
      } catch (e) {
        res = [`err`, e]
      }
      console.log(`res`, res);
    };

    await call(() => bob.checkEq(0, 0));
    await call(() => bob.payMe(10));
    await call(() => bob.noop());
  }
});
