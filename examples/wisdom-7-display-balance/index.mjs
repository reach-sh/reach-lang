import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

if (process.argv.length < 3 || ['seller', 'buyer'].includes(process.argv[2]) == false) {
  console.log('Usage: reach run index [seller|buyer]');
  process.exit(0);
}
const role = process.argv[2];
console.log(`Your role is ${role}`);

const stdlib = loadStdlib(process.env);
console.log(`The consensus network is ${stdlib.connector}.`);
const suStr = stdlib.standardUnit;
const toAU = (su) => stdlib.parseCurrency(su);
const toSU = (au) => stdlib.formatCurrency(au, 4);
const iBalance = toAU(1000);
const showBalance = async (acc) => console.log(`Your balance is ${toSU(await stdlib.balanceOf(acc))} ${suStr}.`);

const commonInteract = {};

// Seller
if (role === 'seller') {
  const sellerInteract = {
    ...commonInteract,
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
  };

};
