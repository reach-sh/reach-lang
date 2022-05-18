import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const SellerContract = SellerBalance.contract(backend);
const BuyerContract = BuyerBalance.contract(backend, SellerContract.getInfo());

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
