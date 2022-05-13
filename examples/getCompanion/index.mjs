import { loadStdlib } from '@reach-sh/stdlib';
import * as backends1 from './build/index.serverMay.mjs';
import * as backendc from './build/index.client.mjs';
const stdlib = loadStdlib();
const [ accA, accB, accC ] = await stdlib.newTestAccounts(3, stdlib.parseCurrency(100));

const go = async (backends) => {
  const ctcA = accA.contract(backends);
  const r = await stdlib.withDisconnect(() =>
    ctcA.p.A({x: 5, ready: stdlib.disconnect}));
  const ctcB = accB.contract(backendc);
  await ctcB.p.A({ r });
};

await go(backends1);
