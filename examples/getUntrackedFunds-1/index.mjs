import {loadStdlib} from '@reach-sh/stdlib';
// import * as ask from '@reach-sh/stdlib/ask.mjs';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob ] =
    await stdlib.newTestAccounts(2, startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const logBalances = async (addrObj) => {
    console.log(`   Alice balance:`, await getBalance(accAlice));
    console.log(`   Bobby balance:`, await getBalance(accBob));
    if (addrObj) {
      console.log(`Contract balance:`, await getBalance({ networkAccount: addrObj}));
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
      gimmeSomeDough: async (address) => {
        // const _ = await ask.ask('Continue? (y/n)', ask.yesno);
        const addrObj = (stdlib.connector === 'ALGO')
                          ? { addr: stdlib.cbr2algo_addr(address) }
                          : { address };
        await stdlib.transfer(accAlice, { networkAccount: addrObj }, stdlib.parseCurrency(50));
        console.log(`Sent some dough to:`, address, addrObj);
        await logBalances(addrObj);
      }
    }),
  ]);

  console.log('Program finished:');
  await logBalances();

})();
