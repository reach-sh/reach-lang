'reach 0.1 exe';

const Seller = participant({
  _product: address,
  _reserve: uint256});

const Buyer = participant({});

const DELAY = 10; // in blocks

function main() {
  const theSeller =
        Seller.join(() => {
          const product = declassify(product);
          const reserve = declassify(reserve); })
        .publish(product, reserve)
        .pay(reserve)
        .timeout(DELAY, _, () => {
          commit();
          return false; } );

  var [ highestBid, highestBidder, active ]
      = [ reserve, theSeller, true ];
  invariant((balance() = highest_bid));
  while ( active ) {
    commit();

    Seller(theSeller).only(() => {
      interact.bidder(highestBid, highestBidder); });

    const newBidder =
          Buyer.join(() => {
            const nextBid = declassify(is(uint256, interact.bid(highestBid)));
            assume(nextBid > highestBid); })
          .publish(nextBid)
          .pay(nextBid)
          .timeout(DELAY, theSeller, () => {
            [ highestBid, highestBidder, active ]
              = [ highestBid, highestBidder, false ];
            continue; } );
    require(nextBid > highestBid);
    transfer(highestBid).to(highestBidder);
    if ( highestBidder != theSeller ) {
      Bidder(highestBidder).leave(false); }
    [ highestBid, highestBidder, active ]
      = [ nextBid, newBidder, true ];
    continue; }

  if ( highestBidder == theSeller ) {
    transfer(highestBid).to(theSeller);
    commit();

    return false; }
  else {
    commit();

    Seller(theSeller).only(() => {
      interact.winner(highestBid, highestBidder);
      const winner = highestBidder; });
    Seller(theSeller).publish(winner)
      .timeout(DELAY, highestBidder, () => {
        transfer(highestBid).to(highestBidder);
        return false; });
    require(winner == highestBidder);
    transfer(highestBid).to(theSeller);
    call(product).go(highestBidder);
    commit();

    return true; } }
