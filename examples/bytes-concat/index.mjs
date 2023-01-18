import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

if ( stdlib.connector !== 'ALGO' ) { process.exit(0); }

const assertEq = (actual, expected) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {expected, actual}, {exps, acts});
  stdlib.assert(exps === acts)
};

const startingBalance = stdlib.parseCurrency(100);
const [ accA ] = await stdlib.newTestAccounts(1, startingBalance);
const ctcA = accA.contract(backend);

await ctcA.p.A({
  x: 'ABCDE',
  y: '012345',
  f: (z) => assertEq(z, 'ABCDE012345'),
  g: (z) => assertEq(z, 'ABCDE012345ABCDE012345ABCDE012345'),
  h: (z) => assertEq(z, 'ABCDE012345ABCDE012345ABCDE012345ABCDE012345ABCDE012345ABCDE012345'),
});
