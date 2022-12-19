import { loadStdlib, test } from '@reach-sh/stdlib';
import * as m1 from './build/index.m1.mjs';
import * as m2 from './build/index.m2.mjs';
const stdlib = loadStdlib();
if (stdlib.connector !== 'ALGO') { process.exit(0); }
const [accA] = await stdlib.newTestAccounts(1, stdlib.parseCurrency(100));

await accA.contract(m2).p.A({});

await test.chkErr('closeOut', 'outstanding box', () => accA.contract(m1).p.A({}));

