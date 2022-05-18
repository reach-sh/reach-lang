'reach 0.1';

// define const and var to be used in the frontend (index.mjs)
const choice = UInt;
const quantity = UInt;
const announcement = Bytes(28);
const product = Object({
  name: Bytes(10),
  unit: Bytes(6),
  units: Bytes(8),
  price: UInt,
});

const products = Array(product, 3);
const commonInteract = {
  showResult: Fun(
    [
      Object({ choice, quantity }),
      Object({
        announcement,
        products,
      }),
    ],
    Null
  ),
};

const sellerInteract = {
  sellerInfo: Object({
    announcement,
    products,
  }),
  reportReady: Fun([announcement, products], Null),
};
const buyerInteract = {
  shop: Fun([Object({ announcement, products })], Object({ choice, quantity })),
};

export const main = Reach.App(() => {
  const Seller = Participant('Seller', {
    ...commonInteract,
    ...sellerInteract,
  });
  const Buyer = Participant('Buyer', {
    ...commonInteract,
    ...buyerInteract,
  });
  init();

  // seller local step
  Seller.only(() => {
    const sellerInfo = declassify(interact.sellerInfo);
  });
  Seller.publish(sellerInfo);
  Seller.interact.reportReady(sellerInfo.announcement, sellerInfo.products);
  commit();

  // Buyer local step
  Buyer.only(() => {
    const decision = declassify(interact.shop(sellerInfo));
  });
  Buyer.publish(decision);
  commit();

  // show result of the transaction
  each([Seller, Buyer], () => interact.showResult(decision, sellerInfo));
});