import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const equals = (a, b) => {
  if (a === b) return true;
  if (a instanceof Date && b instanceof Date)
    return a.getTime() === b.getTime();
  if (!a || !b || (typeof a !== 'object' && typeof b !== 'object'))
    return a === b;
  if (a.prototype !== b.prototype) return false;
  let keys = Object.keys(a);
  if (keys.length !== Object.keys(b).length) return false;
  return keys.every(k => equals(a[k], b[k]));
};

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(100);

  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = alice.deploy(backend);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  const exports = backend.getExports(stdlib);

  const b = stdlib.bigNumberify;

  const a_actual = exports.a;
  const a_expected = [b(1), b(2), [b(3), b(4), [b(5), b(6), b(7)]]];
  console.assert(equals(a_actual, a_expected), "Array does not match expectations", [a_actual, a_expected]);

  const o_actual = exports.o;
  const o_expected = { a: b(5), b: { c: true, d: [b(1), b(2), [true, false]] }}
  console.assert(equals(o_actual, o_expected), "Object does not match expectations", [o_actual, o_expected]);

  console.assert(equals(exports.add1(1), b(2)), "add1(1) did not return 2", []);

  await Promise.all([
    backend.Alice(ctcAlice, {
      ...stdlib.hasRandom
    }),
    backend.Bob(ctcBob, {
      ...stdlib.hasRandom
    }),
  ]);

})();
