import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const approx = (x, n) => x > (n - 3) && x <= n;

const startingBalance = stdlib.parseCurrency(100);

const [ accAlice, accBob, accCreator ] =
  await stdlib.newTestAccounts(3, startingBalance);

const zmd = await stdlib.launchToken(accCreator, 'Zorkmid', 'ZMD');
await accAlice.tokenAccept(zmd.id);
await accBob.tokenAccept(zmd.id);

const transfer = async (to, amt, from = accCreator) => {
  await stdlib.transfer(from, to, stdlib.parseCurrency(amt), zmd.id);
}
await transfer(accAlice, 100);
await transfer(accBob, 100);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const getBalance = async (who) => fmt(await stdlib.balanceOf(who, zmd.id));
const logBalances = async () => {
  console.log(`   Alice balance:`, await getBalance(accAlice));
  console.log(`   Bobby balance:`, await getBalance(accBob));
}

console.log(`Launching program:`);
await logBalances();

const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

await Promise.all([
  backend.Alice(ctcAlice, {
    token: zmd.id,
    ...stdlib.hasConsoleLogger,
  }),
  backend.Bob(ctcBob, {
    gimmeSomeDough: async (addr) => {
      const address = stdlib.formatAddress(addr);
      await transfer(address, 50, accAlice);
      console.log(`Sent some dough to:`, address);
      await logBalances();
    }
  }),
]);

console.log('Program finished:');
await logBalances();
const aliceBal = await getBalance(accAlice);
const bobBal   = await getBalance(accBob);
stdlib.assert(approx(aliceBal, 50));
stdlib.assert(approx(bobBal, 150));
