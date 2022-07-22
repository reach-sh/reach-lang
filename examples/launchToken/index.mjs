import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const [ accA, accB, accC, accD ] = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));

const gil = await stdlib.launchToken(accA, 'Gil', 'GIL', {
  clawback: accB,
  freeze: accC,
  reserve: accD,
  defaultFrozen: true,
});

if ( stdlib.connector === 'ALGO' ) {
  // gh #1270
  console.log(await accA.balancesOf([ parseInt(`${gil.id}`) ]));
}
