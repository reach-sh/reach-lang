import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);

  const ctcAlice = accAlice.deploy(backend);
  const ctcInfo = ctcAlice.getInfo();
  const ctcAddr = ctcAlice.getCtcAddress();
  const ctcBob = accBob.attach(backend, ctcInfo);

  const common = {
    showCtcInfo: async (ctc) => {
      const info = await ctcInfo;
      console.log(`Contract Info From Reach      : ${ctc}`);
      console.log(`Contract Info From ctc.getInfo: ${info}`);
      console.assert(ctc == info);
    },
    showAddress: async (addr) => {
      const ctcAddress = await ctcAddr;
      console.log(`Contract Info From Reach      : ${addr}`);
      console.log(`Contract Info From ctc.getAddress: ${ctcAddress}`);
      console.assert(addr == ctcAddress);
    },
    getCT: async () => { return await ctcInfo; },
    getAddr: async () => { return await ctcAddr; }
  }
  await Promise.all([
    backend.Alice(ctcAlice, common),
    backend.Bob(ctcBob, common),
  ]);

})();
