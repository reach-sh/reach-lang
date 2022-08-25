import { loadStdlib } from '@reach-sh/stdlib';
import * as main1 from './build/index.main1.mjs';
import * as main1_big from './build/index.main1_big.mjs';
import * as main2 from './build/index.main2.mjs';
import * as main3 from './build/index.main3.mjs';

const stdlib = loadStdlib();
const startingBalance = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(startingBalance);
accA.setGasLimit(5_000_000);

const go = async (backend) => {
  const ctcA = accA.contract(backend);
  await ctcA.p.A({ ...stdlib.hasConsoleLogger });
};

await go(main1_big);
await go(main1);
await go(main2);
await go(main3);
