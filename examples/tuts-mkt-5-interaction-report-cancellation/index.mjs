import { loadStdlib, ask } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

if (process.argv.length < 3 || ['seller', 'buyer'].includes(process.argv[2]) == false) {
    console.log(`Usage: reach run index [seller|buyer]`);
    process.exit(0);
}

const role = process.argv[2];
console.log(`Your role is ${role}`);
console.log(`The consensus network is ${stdlib.connector}.`);

const suStr = stdlib.standardUnit;
console.log(`The standard unit is ${suStr}`);
const auStr = stdlib.atomicUnit;
console.log(`The atomic unit is ${auStr}`);
const toAU = (su) => stdlib.parseCurrency(su);
const toSU = (au) => stdlib.formatCurrency(au, 4);
const suBal = 1000;
console.log(`Balance is ${suBal} ${suStr}`);
const auBal = toAU(suBal);
console.log(`Balance is ${auBal} ${auStr}`);
console.log(`Balance is ${toSU(auBal)} ${suStr}`);
console.log(`Balance is ${toSU(auBal)} ${suStr}`);

const commonInteract = {
    reportCancellation: () => {
        console.log(`${role == 'buyer' ? 'You' : 'The buyer'} cancelled the order.`);
    }
};


// Seller
if (role === 'seller') {
    const sellerInteract = {
        ...commonInteract,
        sellerInfo: {
            announcement: 'List of products for sale:',
            products: [
                { name: 'Potatoes', unit: 'bag', units: 'bags', price: toAU(200) },
                { name: 'Carrots', unit: 'bunch', units: 'bunches', price: toAU(100) },
                { name: 'Corn', unit: 'ear', units: 'ears', price: toAU(50) },
            ],
        }
    };

    const acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
    const ctc = acc.contract(backend);
    await ctc.participants.Seller(sellerInteract);

// Buyer
} else {
    const buyerInteract = {
        ...commonInteract,
        shop: async (sellerInfo) => {
            console.log(sellerInfo.announcement);
            sellerInfo.products.forEach((p, i) => {
                console.log(`${i + 1}. ${p.name} at ${toSU(p.price)} ${suStr} per unit (${p.unit}).`);
            });
            const order = { prodNum: 0, prodAmt: 0 };
            const prodNum = await ask(`Enter 1-${sellerInfo.products.length}, or 0 to exit:`, (x => x));
            if (1 <= prodNum && prodNum <= sellerInfo.products.length ) {
                order.prodNum = prodNum;
                order.prodAmt = await ask(`Enter number of units, or 0 to exit:`, (x => x));
                const p = sellerInfo.products[order.prodNum - 1];
                const unitWord = order.prodAmt == 1 ? p.unit : p.units;
                console.log(`You are ordering ${order.prodAmt} ${unitWord} of ${p.name} at ${toSu(p.price)} ${suStr} per ${p.unit}.`);
            }
            return order;
        }
    };
};