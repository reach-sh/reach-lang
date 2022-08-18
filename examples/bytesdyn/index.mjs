import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
if ( stdlib.connector !== 'ETH' ) { process.exit(0); }

const startingBalance = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);

const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());

const run = async (t) => {
  await Promise.all([
    ctcA.p.A({ t }),
    ctcB.p.B({ chk: (y) => {
      console.log({y, t});
      stdlib.assert((y === t), "same");
    } }),
  ]);
};

await run("This is a test");
await run("This is a longer test");
