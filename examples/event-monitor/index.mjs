import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const assertEq = (a, b) => {
  const ja = JSON.stringify(a);
  const jb = JSON.stringify(b);
  if (ja !== jb) {
    throw Error(`Expected ${ja} == ${jb}`);
  }
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);
  const e = ctcAlice.events;

  const SOME  = (e) => [ "Some", e ];
  const LEFT  = (e) => [ "Left", e ];
  const RIGHT = (e) => [ "Right", e ];

  const expectedValues = [
    [  LEFT([ stdlib.bigNumberify(3)
            , stdlib.bigNumberify(1415) ]) ],
    [  LEFT([ stdlib.bigNumberify(4)
            , stdlib.bigNumberify(2000) ]) ],
    [ RIGHT(SOME("THE FUTURE OF BLOCKCHAIN".padEnd(64, '\u0000'))) ],
    [ RIGHT(SOME("IS IN REACH".padEnd(64, '\u0000'))) ],
  ];

  const check = ({ when, what }) => {
    const expected = expectedValues.shift();
    assertEq(what, expected);
    if (expectedValues.length == 0) {
      process.exit(0);
    }
  }

  e.log.monitor(check);

  await Promise.all([
    backend.Alice(ctcAlice, {}),
  ]);
})();
