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
const iBalance = stdlib.parseCurrency(1000);

const acc = await stdlib.newTestAccount(iBalance);

const commonInteract = {
    reportCancellation: () => {
        console.log(`${role == 'buyer' ? 'You' : 'The buyer'} cancelled the order.`);
    },
    reportPayment: (payment) => console.log(`${role == 'buyer' ? 'You' : 'The Buyer'} paid ${toSU(payment)} ${suStr} to the contract.`),
    reportTransfer: (payment) => console.log(`The contract paid ${toSU(payment)} ${suStr} to ${role == 'seller' ? 'you' : 'the seller'}.`),
    reportFulfillment: (p, amt) => {
        const subjectVerb = role == 'seller' ? 'You owe' : 'The seller owes';
        const directObject = role == 'buyer' ? 'you' : 'the buyer';
        console.log(`${subjectVerb} ${directObject} ${amt} ${amt == 1 ? p.unit : p.units} of ${p.name}.`);
    },
    reportExit: () => console.log(`${role == 'buyer' ? 'You' : 'The buyer'} cancelled the order.`),
    statusReport: () => console.log(`${role == 'buyer' ? 'Buyer' : 'Seller'} passes status report.`),
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
            const order = { prodNum: 0, prodAmt: 0 };
            const prodNum = await ask.ask(`Enter 1-${sellerInfo.products.length}, or 0 to exit:`, (x => x));
            if (1 <= prodNum && prodNum <= sellerInfo.products.length ) {
                order.prodNum = prodNum;
                order.prodAmt = await ask.ask(`Enter number of units, or 0 to exit:`, (x => x));
                const p = sellerInfo.products[order.prodNum - 1];
                const unitWord = order.prodAmt == 1 ? p.unit : p.units;
                console.log(`You are ordering ${order.prodAmt} ${unitWord} of ${p.name} at ${toSU(p.price)} ${suStr} per ${p.unit}.`);
            }
            return order;
        },
        confirmPurchase: async (total) => await ask.ask(`Do you want to complete the purchase for ${toSU(total)} ${suStr}?`, ask.yesno),
    };

    const info = await ask.ask(`Paste contract info: `, (s) => JSON.parse(s));
    console.log(`Attaching to contract`);
    console.log(`...`);
    const ctc = acc.contract(backend, info);

    console.log('List of products for sale:');
    console.log('Begin View Section');
    const sellerInfo = await ctc.views.Main.sellerInfo();
    sellerInfo[1].products.forEach((p, i) => {
        console.log(`${i + 1}. ${p.name} at ${toSU(p.price)} ${suStr} per unit (${p.unit}).`);
    });
    console.log('End View Section');
    
    console.log(`Successfully attached`);
    await showBalance(acc);
    await ctc.p.Buyer(buyerInteract);
    await showBalance(acc);

    ask.done();
};