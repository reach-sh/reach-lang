import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const isAlgo = stdlib.connector === 'ALGO';
const [ accA, accB, accC, accD ] = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));

const accOpts = {
  clawback: accB,
  freeze: accC,
  reserve: accD,
};
const gil = await stdlib.launchToken(accA, 'Gil', 'GIL', {
  ...(isAlgo ? {...accOpts, defaultFrozen: true} : {}),
});

if (isAlgo) {
  const bals = await accA.balancesOf([ gil.id ]);
  stdlib.assert(stdlib.bigNumberify('0xffffffffffffffff').eq(bals[0]));
  // gh #1270
  const bals_i = await accA.balancesOf([ parseInt(`${gil.id}`) ]);
  stdlib.assert(bals[0].eq(bals_i[0]));
  // give indexer time to catch up
  const start = (new Date()).valueOf();
  let delta = 0;
  let m = null;
  let e = null;
  while ((delta = (new Date()).valueOf() - start) < 60000) {
    try {
      m = await accA.tokenMetadata(gil.id);
      console.log(`waited an extra ${delta}ms to get token metadata`);
      break;
    } catch (err) { e = err; continue; }
  }
  if (!m) { console.error('Failed to fetch metadata'); throw e; }
  for (const k in accOpts) {
    stdlib.assert(stdlib.addressEq(m[k], accOpts[k]));
  };
  stdlib.assert(m.defaultFrozen === true);
  console.log(`All assertions passed. =]`);
}
