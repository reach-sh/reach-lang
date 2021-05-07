import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.default.mjs';

const assertEq = (l, r) =>
  console.assert(JSON.stringify(l) === JSON.stringify(r));

(async () => {
  const stdlib = await loadStdlib();
  const { winnerIs, ROCK, PAPER, SCISSORS, B_WINS, A_WINS } = backend.getExports(stdlib);

  assertEq(winnerIs(ROCK, PAPER), B_WINS);
  assertEq(winnerIs(SCISSORS, PAPER), A_WINS);
})();
