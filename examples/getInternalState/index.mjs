import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);
const startingBalance = stdlib.parseCurrency(100);

const alice = await stdlib.newTestAccount(startingBalance);
const ctcAlice = alice.contract(backend);

await Promise.all([
  backend.A(ctcAlice, {
    x: 50_000_000_000,
    observe: async () => {
      const st = await ctcAlice.getInternalState();
      const sts = st['1'];

      // Check type
      const expected1 = stdlib.T_Tuple([stdlib.T_Address, stdlib.T_UInt]);
      stdlib.assert(JSON.stringify(sts) == JSON.stringify(expected1));

      // Check toString
      const expected2 = stdlib.connector == 'ALGO'
        ? '(address,uint64)'
        : 'tuple(address,uint256)'
      stdlib.assert(sts.toString() == expected2);
    }
  })
])

