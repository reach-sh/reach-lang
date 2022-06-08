import { loadStdlib, ask } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

if (process.argv.length < 3 || ['seller', 'buyer'].includes(process.argv[2]) == false) {
  console.log('Usage: reach run index [seller|buyer]');
  process.exit(0);
}
const role = process.argv[2];
console.log(`Your role is ${role}`);

const stdlib = loadStdlib(process.env);
const suStr = stdlib.standardUnit;
const toAU = (su) => stdlib.parseCurrency(su);
const toSU = (au) => stdlib.formatCurrency(au, 4);
const iBalance = toAU(1000);
const showBalance = async (acc) => console.log(`Your balance is ${toSU(await stdlib.balanceOf(acc))} ${suStr}.`);
console.log(`The atomic unit is ${stdlib.atomicUnit}`);

const commonInteract = {};

// Seller
if (role === 'seller') {
  const sellerInteract = { 
    ...commonInteract,
    price: toAU(5),
    reportReady: async (price) => {
      console.log(`Your wisdom is for sale at ${toSU(price)} ${suStr}.`);
      console.log(`Contract info: ${JSON.stringify(await ctc.getInfo())}`);
    }
  };
		
  const acc = await stdlib.newTestAccount(iBalance);
  await showBalance(acc);
  const ctc = acc.contract(backend);
  await ctc.participants.Seller(sellerInteract);
  await showBalance(acc);
	
// Buyer
} else {
  const buyerInteract = {
    ...commonInteract,
    confirmPurchase: async (price) => await ask.ask(`Do you want to purchase wisdom for ${toSU(price)} ${suStr}?`, ask.yesno)
  };
  const acc = await stdlib.newTestAccount(iBalance);
  const info = await ask.ask('Paste contract info:', (s) => JSON.parse(s));
  const ctc = acc.contract(backend, info);
  await showBalance(acc);
  await ctc.p.Buyer(buyerInteract);
  await showBalance(acc);
};

ask.done();
