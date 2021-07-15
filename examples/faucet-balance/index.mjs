import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const faucet = await stdlib.getFaucet();
  const balBn = await stdlib.balanceOf(faucet);

  stdlib.assert(stdlib.gt(balBn, 0), `Faucet has funds`);

  const addr = stdlib.formatAddress(faucet);
  const bal = stdlib.formatCurrency(balBn);

  console.log(`${addr} has ${bal} ${stdlib.standardUnit}`);
})();
