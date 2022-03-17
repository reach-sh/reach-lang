import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const accA = await stdlib.newTestAccount(stdlib.parseCurrency(100));
const ctcA = accA.contract(backend);
await ctcA.p.A({});
