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
const auStr = stdlib.atomicUnit;
const toAU = (su) => stdlib.parseCurrency(su);
const toSU = (au) => stdlib.formatCurrency(au, 4);
const suBal = 1000;
console.log(`Balance is ${suBal} ${suStr}`);
const auBal = toAU(suBal);
console.log(`Balance is ${auBal} ${auStr}`);
console.log(`Balance is ${toSU(auBal)} ${suStr}`);

const commonInteract = {};

// Seller
if (role === 'seller') {
  const sellerInteract = { 
    ...commonInteract 
  };
		
  const acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  const ctc = acc.contract(backend);
  await ctc.participants.Seller(sellerInteract)
	
// Buyer
} else {
  const buyerInteract = {
    ...commonInteract 
  };

};
