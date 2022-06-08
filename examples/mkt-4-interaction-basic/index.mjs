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
const showBalance = async (acc) => console.log(`Your Balance is ${toSU(await stdlib.balanceOf(acc))} ${suStr}.`);

const acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));

const commonInteract = {};

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
        },
        reportReady: async (announcement, products) => {
            console.log(`Welcome to the Market`);
            console.log(`Contract info: ${JSON.stringify(await ctc.getInfo(announcement, products))}`);
        }
    };

    await showBalance(acc);
    const ctc = acc.contract(backend);
    await ctc.p.Seller(sellerInteract);
    await showBalance(acc);

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
            return order;
        },
    };

    const info = await ask.ask(`Paste contract info: `, (s) => JSON.parse(s));
    console.log(`Attaching to contract`);
    console.log(`...`);
    const ctc = acc.contract(backend, info);
    console.log(`Successfully attached`);
    await showBalance(acc);
    await ctc.p.Buyer(buyerInteract);
    await showBalance(acc);

    ask.done();
};