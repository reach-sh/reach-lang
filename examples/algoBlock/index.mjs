import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();
if (stdlib.connector !== 'ALGO') { process.exit(0); }
const [accA] = await stdlib.newTestAccounts(1, stdlib.parseCurrency(100));
let theLct;
stdlib.setAdjustTxnParams(async (who, sra, params) => {
  console.log(`adjustTxnParams: `, who, sra, params);
  if ( theLct ) {
    console.log(`Delayed!`);
    return {
      ...params,
      firstRound: theLct,
      lastRound: theLct + 100,
    };
  } else {
    return {
      ...params,
      firstRound: params.firstRound + 1,
      lastRound: params.firstRound + 10,
    };
  }
});
const ctcA = accA.contract(backend);
await ctcA.p.A({
  ...stdlib.hasConsoleLogger,
  delay: async (lct) => {
    theLct = stdlib.bigNumberToNumber(lct);
  }
});
