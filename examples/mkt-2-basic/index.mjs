import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const startingBalance = stdlib.parseCurrency(100);

const SellerBalance = await stdlib.newTestAccount(startingBalance);
const BuyerBalance = await stdlib.newTestAccount(startingBalance);

const SellerContract = SellerBalance.contract(backend);
const BuyerContract = BuyerBalance.contract(backend, SellerContract.getInfo());

const sellerInteract = {
  sellerInfo: {
    announcement: 'List of products for sale:',
    products: [
      { name: 'Potatoes', unit: 'bag', units: 'bags', price: '10' },
      { name: 'Carrots', unit: 'bunch', units: 'bunches', price: '10' },
      { name: 'Corn', unit: 'ear', units: 'ears', price: '5' },
    ],
  },
  reportReady: async (announcement, products) => {
    console.log(`Welcome to the Market`);
    console.log(
      `Contract info: ${JSON.stringify(
        await SellerContract.getInfo(announcement, products)
      )}`
    );
  },
};

const buyerInteract = {
  shop: async (sellerInfo) => {
    console.log(sellerInfo.announcement);
    sellerInfo.products.forEach((product, index) => {
      console.log(
        `${index + 1}. ${product.name} at ${product.price} per ${product.unit}.`
      );
    });

    const choice = Math.floor(Math.random() * 3);
    const quantity = Math.floor(Math.random() * 99);
    console.log(`Buyer wants ${sellerInfo.products[choice].name}`);

    return { choice, quantity };
  },
};

const commonInteract = (person) => ({
  showResult: (decision, sellerInfo) => {
    console.log(
      `${person} agrees to ${person === 'Seller' ? 'sell' : 'buy'} ${
        decision.quantity
      } ${
        decision.quantity > 1
          ? sellerInfo.products[decision.choice].units
          : sellerInfo.products[decision.choice].unit
      } of ${sellerInfo.products[decision.choice].name}`
    );
  },
});

await Promise.all([
  SellerContract.p.Seller({
    ...commonInteract('Seller'),
    ...sellerInteract,
  }),
  BuyerContract.p.Buyer({
    ...commonInteract('Buyer'),
    ...buyerInteract,
  }),
]);
