import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const accAlice = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const ctcAlice = accAlice.contract(backend);

const maxUIntForBits = (bits) => stdlib.bigNumberify(2).pow(bits).sub(1);
const x = 1;
const y = maxUIntForBits(256);

console.log(`x: ${x}`);
console.log(`y: ${y}`);

await ctcAlice.p.Alice({
  x, y,
  truncX: (tx) => {
    console.log(`truncated x: ${tx}`);
    stdlib.assert(tx.eq(x));
  },
  truncY: (ty) => {
    console.log(`truncated y: ${ty}`);
    const expected_y = stdlib.connector == "ALGO" ? maxUIntForBits(64) : y;
    stdlib.assert(ty.eq(expected_y));
  }
});

