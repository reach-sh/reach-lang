'reach 0.1 exe';

const Seller = newParticipant();
const [Bidders] = newParticipantClass();

const DELAY = 10; // in blocks

function main() {
  Seller.only(() => {
    const product = declassify(is(uint256, interact.product()));
    const reserve = declassify(is(uint256, interact.reserve())); });
  Seller.publish(product, reserve)
    .pay(reserve);

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
    .timeout(DELAY, closeTo(Seller, false));

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
      .timeout(DELAY, closeTo(Buyer, false));
    require(checkSign(endorsement, Seller, Buyer));
    transfer(bid).to(Seller);
    call(product).go(endorsement, Buyer);
    commit();

    return true; } }
