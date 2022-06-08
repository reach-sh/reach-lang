"reach 0.1";

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

export const main = Reach.App(() => {
  const Seller = Participant("Seller", {
    ...commonInteract,
    sellerInfo: Object({
      announcement,
      products,
    }),
    reportReady: Fun([announcement, products], Null),
  });
  const Buyer = Participant("Buyer", {
    ...commonInteract,
    shop: Fun(
      [Object({ announcement, products })],
      Object({ choice, quantity })
    ),
  });
  init();

  Seller.only(() => {
    const sellerInfo = declassify(interact.sellerInfo);
  });
  Seller.publish(sellerInfo);
  Seller.interact.reportReady(sellerInfo.announcement, sellerInfo.products);
  commit();

  Buyer.only(() => {
    const decision = declassify(interact.shop(sellerInfo));
  });
  Buyer.publish(decision);
  commit();

  each([Seller, Buyer], () => interact.showResult(decision, sellerInfo));
});
