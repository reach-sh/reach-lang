import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib();
const bal = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(bal);
const ctcA = accA.contract(backend);

try {
  await ctcA.p.A({
    get: async (ib) => {
      const i = stdlib.bigNumberToNumber(ib);
      console.log('get', i);
      switch (i) {
        case 1:
          return 5;
        case 2:
          if ( stdlib.connector === 'ALGO' ) {
            console.log(`Trying to clear state...`);
            const ctcInfo = stdlib.bigNumberToNumber(await ctcA.getInfo());
            const ALGO = stdlib;
            const { algosdk } = ALGO;
            const thisAcc = accA.networkAccount;
            const from = thisAcc.addr;
            const params = await ALGO.getTxnParams('raw');
            const txnApp =
              algosdk.makeApplicationClearStateTxn(from, params, ctcInfo);
            const rtxns = [ txnApp ];
            algosdk.assignGroupID(rtxns);
            const wtxns = rtxns.map(ALGO.toWTxn);
            const tr = await ALGO.signSendAndConfirm( thisAcc, wtxns );
            console.log(`Clear State Result:`, tr);
          }
          return 6;
        default:
          throw Error(`get ${i}`);
      }
    }
  });
} catch (e) {
  const es = e.toString();
  console.log('caught', e, es);
  if ( stdlib.connector === 'ALGO' && es.includes('has not opted in') ) {
    console.log(`Expected error:`, e);
  } else {
    throw e;
  }
}
