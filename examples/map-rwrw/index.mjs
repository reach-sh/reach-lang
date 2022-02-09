import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

  const stdlib = loadStdlib();
  const assertEq = (expected, actual) => {
    const exps = JSON.stringify(expected);
    const acts = JSON.stringify(actual);
    console.log('assertEq', {expected, actual}, {exps, acts});
    stdlib.assert(exps === acts) };
  const startingBalance = stdlib.parseCurrency(100);
  const [ accAlice ] = await Promise.all([
    stdlib.newTestAccount(startingBalance),
  ]);
  accAlice.setDebugLabel('Alice');
  const ctcAlice = accAlice.contract(backend);

  const common = (Who) => ({
    get: (() => Who.length),
    check: assertEq,
  });
  console.log(`BEGIN map-rwrw`);
  await Promise.all([
    backend.Alice(ctcAlice, common('Alice')),
  ]);
