import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const assertEq = (l, r) => {
  const ls = JSON.stringify(l);
  const rs = JSON.stringify(r);
  if (ls != rs) {
    throw Error(`Assertion failed! ${ls} != ${rs}`);
  }
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);

  await Promise.all([
    backend.Alice(ctcAlice, {
      observe: async () => {
        const uv = await ctcAlice.unsafeViews.t();
        const exp = [stdlib.bigNumberify(4), false];
        assertEq(uv, exp);

        // ensure unset view throws error
        let errorThrown = false;
        try {
          await ctcAlice.unsafeViews.u();
        } catch (e) {
          errorThrown = true;
        }
        assertEq(errorThrown, true);
      }
    }),
  ]);
})();
