import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

  const startingBalance = stdlib.parseCurrency(0);
  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);
  try {
    await stdlib.transfer(accAlice, accBob, stdlib.parseCurrency(1));
  } catch (e) {
    if ( (stdlib.connector === 'CFX' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('timeout in mining')))
      || (stdlib.connector === 'ETH' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('insufficient funds'))) 
      || (stdlib.connector === 'ALGO' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('overspend'))) ) {
      console.log(`Got an unexpected exn`);
      throw e;
    }
    console.log(`Got the expected exn`);
    return;
  }
  throw Error('Expected exn, but none thrown.');
