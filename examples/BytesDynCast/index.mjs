import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

if ( stdlib.connector === 'ALGO' ) { process.exit(0); }

const startingBalance = stdlib.parseCurrency(100);
const [ accA, accB ] = await stdlib.newTestAccounts(2, startingBalance);

const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());
const iface = { post: () => "hiya", }
await Promise.all([
  ctcA.p.A(iface),
  ctcB.p.B(iface),
]);
const e1 = await ctcA.e.e.next();
const e2 = await ctcA.e.e.next();
console.log(e1)
console.log(e2)
stdlib.assert(e1.what[0], "e1")
stdlib.assert(! e2.what[0], "e2")

