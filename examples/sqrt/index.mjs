import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const bal = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(bal);
const ctcA = accA.contract(backend);
await ctcA.p.A({
  x: stdlib.bigNumberify('9234'),
  y: stdlib.bigNumberify('96'),
  x256: stdlib.bigNumberify('92348427152342134'),
  y256: stdlib.bigNumberify('303888840'),
});
