import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
if ( stdlib.connector === 'ALGO' ) { process.exit(0); }

const [ accA, accB ] =
  await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));

const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());

await Promise.all([
  ctcA.participants.A({
    x: 5,
  }),
  ctcB.p.B({
    ...stdlib.hasConsoleLogger,
  }),
]);
