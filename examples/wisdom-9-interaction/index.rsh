'reach 0.1';

const commonInteract = {};
const sellerInteract = {
  ...commonInteract,
  price: UInt,
  reportReady: Fun([UInt], Null),
};
const buyerInteract = {
  ...commonInteract,
  confirmPurchase: Fun([UInt], Bool)
};

export const main = Reach.App(() => {
  const S = Participant('Seller', sellerInteract);
  const B = Participant('Buyer', buyerInteract);
  init();

  S.only(() => { const price = declassify(interact.price); });
  S.publish(price);
  S.interact.reportReady(price);
  commit();

  B.only(() => { const willBuy = declassify(interact.confirmPurchase(price)); });
  B.publish(willBuy);
  if (!willBuy) {
    commit();
  } else {
    commit();
  }

  exit();
});
