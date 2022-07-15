import { loadStdlib, util } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);

const [accAlice, accBob] =
  await stdlib.newTestAccounts(2, startingBalance);

const ctcAlice = accAlice.contract(backend);

await Promise.all([
  backend.A(ctcAlice, {
    checkView: async () => {
      const x = await ctcAlice.unsafeViews.x();
      console.log(`x:`, x);
      stdlib.assert(x == 43);

      const val = await ctcAlice.unsafeViews.val();
      console.log(`val:`, val);
      stdlib.assert(val == 43);

      const val2 = await ctcAlice.unsafeViews.val2();
      console.log(`val2:`, val2);
      stdlib.assert(val2 == 43);

      const fr = await ctcAlice.unsafeViews.f(1);
      console.log(`fr:`, fr);
      stdlib.assert(fr == 2);

      const succr = await ctcAlice.unsafeViews.succ(1);
      console.log(`succr:`, succr);
      stdlib.assert(succr == 2);

      const y = await ctcAlice.unsafeViews.V2.y();
      console.log(`y:`, y);
      stdlib.assert(y == 43);

      const yVal = await ctcAlice.unsafeViews.V2.yVal();
      console.log(`yVal:`, yVal);
      stdlib.assert(yVal == 43);

      const gr = await ctcAlice.unsafeViews.V2.g(1);
      console.log(`gr:`, gr);
      stdlib.assert(gr == 2);

      const subr = await ctcAlice.unsafeViews.V2.sub(1);
      console.log(`subr:`, subr);
      stdlib.assert(subr == 2);
    }
  }),
]);

