import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const assertEq = (expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  const msg = JSON.stringify(['assertEq', {expected, actual}, {exps, acts}]);
  stdlib.assert(exps === acts, msg); };

(async () => {
  const now = await stdlib.getNetworkTime();
  stdlib.setQueryLowerBound(now);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));
  accAlice.setDebugLabel('Alice');
  accBob.setDebugLabel('Bob');
  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const LOG = {};
  const mkPart = (who) => {
    LOG[who] = [];
    return {
      log: (lab, bn) => {
        const n = stdlib.bigNumberToNumber(bn);
        LOG[who].push([lab, n]);
        console.log(who, lab, n);
      },
      deadline: ({
        CFX: 100,
        ETH: 10,
        ALGO: 10,
      })[stdlib.connector],
    };
  };
  await Promise.all([
    ctcAlice.p.Alice({
      ...mkPart('Alice'),
    }),
    ctcBob.p.Bob({
      ...mkPart('Bob'),
    }),
  ]);
  assertEq(LOG['Alice'], LOG['Bob']);
})();
