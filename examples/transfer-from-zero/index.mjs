import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib({ ALGO_NODE_WRITE_ONLY: 'yes' });
const expectFail = async (name, f) => {
  try {
    await f();
    throw 42;
  } catch (e) {
    if ( e === 42 ) {
      throw Error(`${name} succeeded, but shouldn't`);
    }
    console.log(`Got error`, e);
  }
};

const accMt = await stdlib.createAccount();
console.log(await stdlib.balanceOf(accMt));
await expectFail('tokenMetadata(42)', () => accMt.tokenMetadata(42));

const startingBalance = stdlib.parseCurrency(0);
const [ accAlice, accBob ] =
  await stdlib.newTestAccounts(2, startingBalance);

try {
  await stdlib.transfer(accAlice, accBob, stdlib.parseCurrency(1));
  throw Error('Expected exn, but none thrown.');
} catch (e) {
  if ( (stdlib.connector === 'CFX' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('timeout in mining')))
    || (stdlib.connector === 'ETH' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('insufficient funds'))) 
    || (stdlib.connector === 'ALGO' && !(typeof e.message === 'string' && e.message.toLowerCase().includes('overspend'))) ) {
    console.log(`Got an unexpected exn`);
    throw e;
  }
  console.log(`Got the expected exn`);
}
