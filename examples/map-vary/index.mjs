import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

  const stdlib = loadStdlib();
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice, accBob ] = await Promise.all([
    stdlib.newTestAccount(startingBalance),
    stdlib.newTestAccount(startingBalance),
  ]);
  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const common = (Who) => ({
    get: (() => Who.length),
    check: assertEq,
  });
  await Promise.all([
    backend.Alice(ctcAlice, common('Alice')),
    backend.Bob(ctcBob, common('Bob')),
  ]);
