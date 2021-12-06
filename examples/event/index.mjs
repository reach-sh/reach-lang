import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const thread = async (f) => await f();

const assertEq = (a, b) => {
  if (a !== b) {
    throw Error(`Expected ${a} == ${b}`);
  }
}

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const ctcAlice = accAlice.contract(backend);
  const e = ctcAlice.events;
  console.log(e);

  let x = stdlib.bigNumberify(0);
  let lastTime = stdlib.bigNumberify(0);

  await Promise.all([
    backend.A(ctcAlice, {
      getX: () => {
        x = x.add(1);
        return x;
      },
      checkX: async () => {
        const { when, what } = await e.x_event_x.next();
        lastTime = when;
        console.log(what);
        console.log(x);
        // assertEq(what, x);
      },
      checkY: async () => {
        const { when, what } = await e.x_event_y.next();
        lastTime = when;
        console.log(what);
        // assertEq(what[0], x);
        // assertEq(what[1], x);
      },
      loopCont: async () => {
        e.x_event_x.seek(lastTime.add(1));
        e.x_event_y.seek(lastTime.add(1));
      }
    }),
  ]);
})();
