// Ensures SMT errors can parse pure Array types,
// which appear nested inside of other parsed types.
"reach 0.1";

const AuctionProps = {
  arbitrator: Array(Address, 3),
  assetId: Token,
  price: UInt,
  quantity: UInt,
};

const BuyerProps = {
  getPurchaseAmount: Fun([UInt, UInt], UInt),
  showSaleItem: Fun([], Null),
};

const OwnerInterface = {
  getSaleProps: Fun([], Object(AuctionProps)),
  ...BuyerProps,
};

export const main = Reach.App(() => {
  const Owner = Participant("Owner", OwnerInterface);
  const Buyer = ParticipantClass("Buyer", BuyerProps);
  deploy();

  // Set up sale-item details
  Owner.only(() => {
    const sale = declassify(interact.getSaleProps());
    const token = sale.assetId;
  });
  // Pay token to contract
  Owner.publish(sale, token).pay([[sale.quantity, token]]);
  commit();

  Buyer.publish();

  var inStock = sale.quantity;
  invariant(balance(token) == inStock);
  while (inStock > 0) {
    commit();

    Buyer.only(() => {
      // Show what's on sale (allows the front-end to use vNFTSale)
      interact.showSaleItem();
      const qty = declassify(interact.getPurchaseAmount(inStock, sale.price));
      assume(qty > 0, "You need to purchase at least 1 item!");
      const buyerPmt = qty * sale.price;
      assume(qty <= inStock, "There aren't that many items left!");
    });

    // Only attempt to purchase when buyer wants more than 0 items
    Buyer.publish(qty, buyerPmt)
      .when(buyerPmt > 0)
      .pay(buyerPmt)
      .timeout(false);

    // Transfer quantity of purchased NFTs to buyer
    transfer(qty, token).to(this);

    // Payout participants => This is likely where the rage occurs.
    // The exact same chunk works in a `parallelReduce`

    const arbFee = (buyerPmt * (2 / 100)) / Array.length(sale.arbitrator);
    Array.forEach(sale.arbitrator, (a) => {
      transfer(arbFee).to(a);
    });
    transfer(balance()).to(Owner);

    // Update number of items left to sell
    const inventory = inStock - qty;
    inStock = inventory;
    continue;
  }

  commit();
  exit();
});

