import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const thread = async (f) => await f();

const assertEq = (l, r) => {
  const ls = JSON.stringify(l);
  const rs = JSON.stringify(r);
  if (ls != rs) {
    throw Error(`Assertion failed! ${ls} != ${rs}`);
  }
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);

  const ctcAlice = accAlice.contract(backend);

  const user = async (uid, exp) => {
    const acc = await stdlib.newTestAccount(startingBalance);
    acc.setDebugLabel(uid);
    return async () => {
      const ctc = acc.contract(backend, ctcAlice.getInfo());
      const go = ctc.safeApis.go;
      const res = await go();
      console.log(`res`, res);
      assertEq(res, exp);
    };
  };

  await Promise.all([
    backend.Alice(ctcAlice, {
      go: async () => {
        thread(await user('Bob', ['None', null]));
        return false;
      }
    }),
    thread(await user('Bob', ['Some', true])),
  ]);

})();
