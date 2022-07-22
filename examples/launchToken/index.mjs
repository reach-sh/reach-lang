import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const [ accA, accB, accC, accD ] = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));

await stdlib.launchToken(accA, 'Gil', 'GIL', {
  clawback: accB,
  freeze: accC,
  reserve: accD,
  defaultFrozen: true,
});
