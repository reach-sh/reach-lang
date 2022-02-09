import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const assertEq = (l, r) => {
  const ls = JSON.stringify(l);
  const rs = JSON.stringify(r);
  if (ls != rs) {
    throw Error(`Assertion failed! ${ls} != ${rs}`);
  }
}

const startingBalance = stdlib.parseCurrency(100);
const [ accAlice ] =
  await stdlib.newTestAccounts(1, startingBalance);
const ctcAlice = accAlice.contract(backend);
const user = async (uid, exp) => {
  const acc = await stdlib.newTestAccount(startingBalance);
  acc.setDebugLabel(uid);
  const ctc = acc.contract(backend, ctcAlice.getInfo());
  const go = ctc.safeApis.go;
  const res = await go();
  console.log(`res`, res);
  assertEq(res, exp);
};
await ctcAlice.p.Alice({
  launchBob: async (i, exp) => {
    await user(`Bob${i}`, exp);
  }
});
