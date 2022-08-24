import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
if ( stdlib.connector !== 'ETH' ) { process.exit(0); }

const startingBalance = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, startingBalance);

const run = async (base) => {
  const ctcA = accA.contract(backend);
  await Promise.all([
    ctcA.p.A({
      base,
      ready: async (info) => {
        const ctcB = accB.contract(backend, info);
        const chk = async (i) => {
          const v = await ctcB.unsafeViews.read(i);
          const jv = `${base}${i}.jpg`;
          console.log({ i, v, jv });
          stdlib.assert((v === jv), "same");
        };
        await chk(0);
        await chk(1);
        await chk(32);
        await chk(256);
      },
    }),
  ]);
};

await run("http://weirdmonkey.com/nifty/");
