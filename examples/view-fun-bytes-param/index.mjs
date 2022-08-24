// This example demonstrates that the bug associated with core-2064 has been fixed

import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const a = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const c = a.contract(backend);

await stdlib.withDisconnect(() => c.p.A({ dc: stdlib.disconnect }));

const test = async (byte) => {
  const result = (await c.v.identityViewFun(byte))[1];
  console.log(`${byte} === ${result}`);
  stdlib.assert(byte === result);
};

for (const c of "some test input!") {
  await test(c);
}
