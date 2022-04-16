import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const bal = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(bal);
const ctcA = accA.contract(backend);
await ctcA.p.A({});
