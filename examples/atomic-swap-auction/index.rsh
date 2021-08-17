'reach 0.1';
'use strict';

const common = {
  showOutcome: Fun([Address], Null)
};

export const main = Reach.App(() => {
  const Auctioneer =  Participant('Auctioneer', {
    ...common,
    getSwap: Fun([], Tuple(Token, UInt, Token, UInt, UInt)),
    showAuctionStart: Fun([], Null),
  });

  const Bidder =  ParticipantClass('Bidder', {
    ...common,
    getBid: Fun([UInt], Maybe(UInt)),
    showBid: Fun([Bool, Maybe(UInt)], Null),
  });

  deploy();

  Auctioneer.only(() => {
    const [ tokA, amtA, tokB, reservePrice, timeout ] = declassify(interact.getSwap());
    assume(tokA != tokB);
  });
  Auctioneer
    .publish(tokA, amtA, tokB, reservePrice, timeout)
    .pay([ [amtA, tokA ]]);

  var [ dealMade, tries ] = [ false, 0 ];
  invariant(balance(tokA) == (dealMade ? 0 : amtA) && balance(tokB) == 0);
  while (!dealMade && tries < 3) {

    commit();
    Anybody.publish();

    const [ timeRemaining, keepGoing ] = makeDeadline(timeout);

    const [ highestBidder, isFirstBid, currentPrice ] =
      parallelReduce([ Auctioneer, true, reservePrice ])
        .invariant(balance(tokA) == amtA && balance(tokB) == (isFirstBid ? 0 : currentPrice))
        .while(keepGoing())
        .paySpec([ tokB ])
        .case(Bidder,
          (() => {
            const mbid = highestBidder != this
              ? declassify(interact.getBid(currentPrice))
              : Maybe(UInt).None();
            return ({
              when: maybe(mbid, false, ((b) => b > currentPrice)),
              msg : fromSome(mbid, 0)
            });
          }),
          ((bid) => [ 0, [bid, tokB] ]),
          ((bid) => {
            require(bid > currentPrice);
            transfer(isFirstBid ? 0 : currentPrice, tokB).to(highestBidder);
            return [ this, false, bid ];
          }))
        .timeRemaining(timeRemaining());
    commit();

    Auctioneer.publish();
    transfer(isFirstBid ? 0 : amtA, tokA).to(highestBidder);
    transfer(isFirstBid ? 0 : currentPrice, tokB).to(Auctioneer);

    each([Auctioneer, Bidder], () => interact.showOutcome(highestBidder));

    [ dealMade, tries ] = [ !isFirstBid, tries + 1 ];
    continue;
  }
  commit();

  Auctioneer.publish();
  transfer(balance()).to(Auctioneer);
  transfer(balance(tokA), tokA).to(Auctioneer);
  commit();

  exit();
});
