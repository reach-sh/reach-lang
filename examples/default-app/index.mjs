import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.default.mjs';

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
  const exports = backend.getExports(stdlib);

  console.assert(equals(exports.winnerIs(exports.ROCK, exports.PAPER), exports.B_WINS));
  console.assert(equals(exports.winnerIs(exports.SCISSORS, exports.PAPER), exports.A_WINS));
})();
