import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const assertEq = (l, r) => {
  if (JSON.stringify(l) != JSON.stringify(r)) {
    throw Error(`Assertion failed!`);
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
