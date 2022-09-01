import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
if ( stdlib.connector !== 'ETH' ) { process.exit(0); }

const startingBalance = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);

const run_ = async (t, shouldPassArray) => {
  const ctcA = accA.contract(backend);
  const ctcB = accB.contract(backend, ctcA.getInfo());
  //ctcA.e.u.monitor((evt) => console.log(`LOOK! u is`, evt));
  await Promise.all([
    ctcA.p.A({
      // Tests that we can pass strings and byte arrays to interact
      t: shouldPassArray
        ? stdlib.ethers.utils.toUtf8Bytes(t)
        : t
    }),
    ctcB.p.B({ chk: (y) => {
      console.log({y, t});
      stdlib.assert((y === t), "same");
    } }),
  ]);
};

const run = async (t) => {
  await run_(t, true);
  await run_(t, false);
}

await run("This is a test");
await run("This is a longer test");
