import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = await loadStdlib();

const bn = stdlib.bigNumberify;
const pc = stdlib.parseCurrency;

const acc = await stdlib.newTestAccount(pc(100));
const ctc = acc.contract(backend);

let key1 = 0;
let key2 = false;
let val = 0;

await Promise.all([
  backend.A(ctc, {
    getKey: () => {
      key1++;
      key2 = !key2;
      return [ bn(key1), key2 ];
    },
    lookup: () => {
      return [ bn(key1), key2 ];
    },
    getValue: () => {
      val++;
      return bn(val);
    },
    chk: (i) => {
      stdlib.assert(i[1].eq(bn(val)));
    }
  }),
]);
