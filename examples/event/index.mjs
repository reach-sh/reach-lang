import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const assertEq = (a, b) => {
  if (!a.eq(b)) {
    throw Error(`Expected ${JSON.stringify(a)} == ${JSON.stringify(b)}`);
  }
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);
  const e = ctcAlice.e;

  let x = stdlib.bigNumberify(0);

  const getLog = (f) => async () => {
    const { when, what } = await f.next();
    const lastTime = await f.lastTime();
    assertEq(lastTime, when);
    return what;
  }

  const getXLog = getLog(e.x_event.x);
  const getYLog = getLog(e.x_event.y);

  await Promise.all([
    backend.A(ctcAlice, {
      getX: () => x = x.add(1),
      checkX: async (isHardcoded) => {
        const what   = await getXLog();
        const expect = isHardcoded ? stdlib.bigNumberify(4) : x;
        assertEq(what[0], expect);
      },
      checkY: async (isHardcoded) => {
        const what    = await getYLog();
        const expect0 = x;
        const expect1 = isHardcoded ? stdlib.bigNumberify(2) : x;
        assertEq(what[0], expect0);
        assertEq(what[1], expect1);
      }
    }),
  ]);
})();
