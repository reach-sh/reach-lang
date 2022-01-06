import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const approx = (x, n) => x > (n - 3) && x < n;

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const logBalances = async (address) => {
    console.log(`   Alice balance:`, await getBalance(accAlice));
    console.log(`   Bobby balance:`, await getBalance(accBob));
    if (address) {
      console.log(`Contract balance:`, await getBalance(address));
    }
  }

  console.log(`Launching program:`);
  await logBalances();

  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  await Promise.all([
    backend.Alice(ctcAlice, {
    }),
    backend.Bob(ctcBob, {
      gimmeSomeDough: async (addr) => {
        const address = stdlib.formatAddress(addr);
        await stdlib.transfer(accAlice, address, stdlib.parseCurrency(50));
        console.log(`Sent some dough to:`, address);
        await logBalances(address);
      }
    }),
  ]);

  console.log('Program finished:');
  await logBalances();
  const aliceBal = await getBalance(accAlice);
  const bobBal   = await getBalance(accBob);
  stdlib.assert(approx(aliceBal, 50));
  stdlib.assert(approx(bobBal, 150));

})();
