import {loadStdlib, test} from '@reach-sh/stdlib';
import * as backendC1 from './build/index.mainC1.mjs';
import * as backendC2 from './build/index.mainC2.mjs';
import * as backendS from './build/index.mainS.mjs';

const stdlib = loadStdlib();
if (stdlib.connector !== 'ALGO') { process.exit(0); }

const sBal = stdlib.parseCurrency(100);
const [ accA, accB ] =
  await stdlib.newTestAccounts(2, sBal);

const tokZ = await stdlib.launchToken(accA, 'zorkmid', 'ZMD');
const tok1 = tokZ.id;
const tokR = await stdlib.launchToken(accA, 'gil', 'GIL');
const tok2 = tokR.id;

const go = async (acc, who) => {
  acc.setDebugLabel(who);
  acc.setGasLimit(5000000);
  await acc.tokenAccept(tok1);
  await acc.tokenAccept(tok2);
  await tokZ.mint(acc, sBal);
  await tokR.mint(acc, sBal);
  return async () => {
    console.log(who, 'balance', await stdlib.balancesOf(acc, [ null, tok1, tok2 ]));
  };
};
const showA = await go(accA, 'A');
const showB = await go(accB, 'B');
const show = async () => {
  await showA();
  await showB();
};

await show();
const x = stdlib.parseCurrency('4.2');

const goC = async (backendC) => {
  const ctcA = accA.contract(backendS);
  console.log('A starting');
  await ctcA.p.D({ x, tok1, tok2,
    ready: async (serverInfo) => {
      console.log('A ready with', serverInfo);
      const ctcB = accB.contract(backendC);
      console.log('B starting with', serverInfo);
      await ctcB.p.D({ serverInfo, x, tok1, tok2,
        inform: async (...args) => {
          console.log('B saw', ...args);
        }
      });
      console.log('B done');
    },
  });
  console.log('A done');
  await show();
};

test.one('works', async () => {
  await goC(backendC1);
});

test.one('doesnt work', async () => {
  await test.chkErr('wrong txn order', 'logic eval error', () =>
    goC(backendC2));
});

await test.run({ noVarOutput: true });
