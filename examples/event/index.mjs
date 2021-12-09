import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const thread = async (f) => await f();

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
  const e = ctcAlice.events;

  let x = stdlib.bigNumberify(0);
  let lastTime = stdlib.bigNumberify(0);

  const getLog = (f) => async () => {
    const { when, what } = await f.next();
    lastTime = when;
    return what;
  }

  const getXLog = getLog(e.x_event_x);
  const getYLog = getLog(e.x_event_y);

  await Promise.all([
    backend.A(ctcAlice, {
      getX: () => {
        x = x.add(1);
        return x;
      },
      checkX: async (isHardcoded) => {
        const what = await getXLog();
        assertEq(what[0], isHardcoded ? stdlib.bigNumberify(4) : x);
      },
      checkY: async (isHardcoded) => {
        const what = await getYLog();
        assertEq(what[0], x);
        assertEq(what[1], isHardcoded ? stdlib.bigNumberify(2) : x);
      },
      loopCont: async () => {
        e.x_event_x.seek(lastTime.add(1));
        e.x_event_y.seek(lastTime.add(1));
      }
    }),
  ]);
})();
