import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);
const pc = stdlib.parseCurrency;
const startingBalance = pc(100);

(async () => {
  const [ accAlice ] =
    await stdlib.newTestAccounts(1, startingBalance);

  const zmd = await stdlib.launchToken(accAlice, "Zorkmid", "ZMD");
  const gil = await stdlib.launchToken(accAlice, "Gil", "GIL");

  const ctcAlice = accAlice.contract(backend);

  await Promise.all([
    backend.Alice(ctcAlice, {
      params: [ zmd.id, gil.id, pc(10) ],
      checkBal: (i, b1, b2) => {
        const exp = (i == 0)
          ? [ pc(10), pc(10) ]
          : [ pc(0), pc(10) ];
        const exp_ = JSON.stringify(exp);
        const act_ = JSON.stringify([ b1, b2 ]);
        stdlib.assert(exp_ == act_, `${exp_} == ${act_}`);
      }
    }),
  ]);
})();
