import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();

const pc = stdlib.parseCurrency;
const sb = pc(100);
const bn = stdlib.bigNumberify;
const accAlice = await stdlib.newTestAccount(sb);
const ctcAlice = accAlice.contract(backend);

const s = (v) => ['Some', v];

const x = bn(4);
const b = "Hello Moto";
const m = s({ b: true });
const v1 = bn(1);
const v2 = bn(2);
const v3 = bn(3);

await Promise.all([
  backend.A(ctcAlice, {
    x,
    b,
    m,
    v: [v1, v2, v3],
    chk: (vp) => {
      const act = JSON.stringify(vp);
      console.log(`act:`, act);
      const exp = JSON.stringify([ s(v1), s(v2), s(v3) ]);
      console.log(`exp:`, exp);
      stdlib.assert(act == exp);
    }
  })
])
console.log('Done');
