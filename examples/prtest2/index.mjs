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
  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const LOG = {
    'expected': [
      ['start', 0],
      ['while-after',0],
      ['while-after',1],
      ['while-after',2],
      ['while-after',3],
      ['while-after',4],
      ['end',5],
    ],
  };
  const checkLog = (who) => assertEq(LOG['expected'], LOG[who]);
  const mkPart = (who) => {
    LOG[who] = [];
    return {
      log: (lab, bn) => {
        const n = stdlib.bigNumberToNumber(bn);
        LOG[who].push([lab, n]);
        console.log(who, lab, n);
      },
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
  checkLog('Alice');
  checkLog('Bob');
})();
