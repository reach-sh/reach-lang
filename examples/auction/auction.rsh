'reach 0.1 exe';

const Seller = participant({
  _product: address,
  _reserve: uint256});

const [Bidders] = participant({});

const DELAY = 10; // in blocks

function main() {
  Seller.only(() => {
    const product = declassify(product);
    const reserve = declassify(reserve); });
  Seller.publish(product, reserve)
    .pay(reserve)
    .timeout(DELAY, _, () => {
      commit();
      return false; } );

  Bidders.only(() => {
    const bid = declassify(is(uint256, interact.bid())); });
  const Buyer =
    select(Bidders
           .publish(bid)
           .pay(bid))
    .choose(invariant( balance() == reserve + bid ),
            () => {
              if ( next.bid > prev.bid ) {
                transfer(prev.bid).to(prev);
                return next; }
              else {
                transfer(next.bid).to(next);
                return prev; } })
    .until(DELAY)
    .timeout(Seller, () => {
      transfer(reserve).to(Seller);
      commit();
      return false; });

  if ( bid < reserve ) {
    transfer(bid).to(Buyer);
    transfer(reserve).to(Seller);
    commit();
    return false; }
  else {
    transfer(reserve).to(Seller);
    commit();

    Seller.only(() => {
      interact.winner(bid, Buyer);
      const endorsement = sign(Seller, Buyer); });
    Seller.publish(endorsement)
      .timeout(DELAY, Buyer, () => {
        transfer(bid).to(Buyer);
        return false; });
    require(check_sign(endorsement, Seller, Buyer));
    transfer(bid).to(Seller);
    call(product).go(endorsement, Buyer);
    commit();

    return true; } }
