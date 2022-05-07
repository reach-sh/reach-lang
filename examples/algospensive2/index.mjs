import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
const [ accA ] = await stdlib.newTestAccounts(1, stdlib.parseCurrency(100));
accA.setDebugLabel('Alice');
const ctcA = accA.contract(backend);
await ctcA.p.A({ x: 5 });
