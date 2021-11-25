import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);
  accAlice.setDebugLabel('Alice');
  accBob.setDebugLabel('Bob');

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  const common = (ctcMe) => ({
    showCtcInfo: async (ctc) => {
      const info = await ctcMe.getInfo();
      console.log(`Contract Info From Reach      : ${ctc}`);
      console.log(`Contract Info From ctc.getInfo: ${info}`);
      stdlib.assert(ctc == info);
    },
    showAddress: async (addr) => {
      const ctcAddress = await ctcMe.getContractAddress();
      console.log(`Address Info From Reach         : ${addr}`);
      console.log(`Address Info From ctc.getAddress: ${ctcAddress}`);
      stdlib.assert(addr == ctcAddress);
    },
    getCT: async () => {
      console.log(`getCT`);
      const r = await ctcMe.getInfo();
      console.log(`getCT`, r);
      return r;
    },
    getAddr: async () => {
      console.log(`getAddr`);
      const r = await ctcMe.getContractAddress();
      console.log(`getAddr`, r);
      return r;
    },
  });
  await Promise.all([
    ctcAlice.p.Alice({
      ...common(ctcAlice),
    }),
    ctcBob.p.Bob({
      ...common(ctcBob),
      doTransfer: async (A) => {
        console.log(`Bob sends funds to Alice`);
        await stdlib.transfer(accBob, stdlib.formatAddress(A), stdlib.parseCurrency(1));
      },
    }),
  ]);

})();
