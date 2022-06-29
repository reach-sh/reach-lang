'reach 0.1';

const commonInteract = {
  reportCancellation: Fun([], Null),
  reportTransfer: Fun([UInt], Null),
  reportPayment: Fun([UInt], Null),
};
const sellerInteract = {
  ...commonInteract,
  price: UInt,
  wisdom: Bytes(128),
  reportReady: Fun([UInt], Null),
};
const buyerInteract = {
  ...commonInteract,
  confirmPurchase: Fun([UInt], Bool),
  reportWisdom: Fun([Bytes(128)], Null)
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
    each([S, B], () => interact.reportCancellation());
    exit();
  } else {
    commit();
  }

  B.pay(price);
  each([S, B], () => interact.reportPayment(price));
  commit();

  S.only(() => { const wisdom = declassify(interact.wisdom); });
  S.publish(wisdom);
  transfer(price).to(S);
  commit();

  each([S, B], () => interact.reportTransfer(price));
  B.interact.reportWisdom(wisdom);

  exit();
});
