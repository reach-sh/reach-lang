// index.mjs
import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib(process.env);
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(100);
  const alice = await stdlib.newTestAccount(startingBalance);
  const ctcAlice = alice.deploy(backend);

  const get = () => '';
  const put = (act, exp, rcmp) => {
    const jcmp = stdlib.bytesEq(act, exp);
    console.log({act, exp, rcmp, jcmp});
    stdlib.assert(rcmp === jcmp, "R = J");
    stdlib.assert(rcmp, "R");
  };
  const check = async (expected) => {
    assertEq(['Some', expected], await ctcAlice.getViews().Debug.vals());
  };
  await Promise.all([
    backend.Alice(ctcAlice, {
      get9: get,
      getA: get,
      check,
      put,
    }),
  ]);
})();
