import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(0);

  console.log('Creating accounts w/ 0 balance...');
  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);
  console.log('Hello, Alice and Bob!');

  let e = null;
  try {
    await Promise.race([
      stdlib.transfer(accAlice, accBob, stdlib.parseCurrency(1)),
      new Promise((resolve, reject) => {
        setTimeout(
          () => {
            reject('timeout; exn took too long to throw');
          },
          1000,
        );
      }),
    ]);
  } catch (_e) {
    e = _e;
  }

  if (e) {
    // TODO: Add more connector-specific inspections here as we add more connectors
    if (stdlib.connector === 'CFX' || stdlib.connector === 'ETH') {
      if (!(typeof e.message === 'string' && e.message.toLowerCase().includes('insufficient funds'))) {
        console.log(`CFX|ETH: Got an unexpected exn`);
        throw e;
      }
    } else if (stdlib.connector === 'ALGO') {
      if (!(typeof e.message === 'string' && e.message.toLowerCase().includes('overspend'))) {
        console.log(`ALGO: Got an unexpected exn`);
        throw e;
      }
    }
    console.log(`Got the expected exn`);
  } else {
    throw Error('Expected exn, but none thrown.');
  }

  console.log('Goodbye, Alice and Bob!');
})();
