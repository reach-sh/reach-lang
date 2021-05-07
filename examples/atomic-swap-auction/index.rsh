'reach 0.1';
'use strict';

const common = {
  showOutcome: Fun([Maybe(Address)], Null)
};

export const main = Reach.App(
  { },
  [ Participant('Auctioneer', {
      ...common,
      getSwap: Fun([], Tuple(Token, UInt, Token, UInt, UInt)),
      showAuctionStart: Fun([], Null),
    }),
    ParticipantClass('Bidder', {
      ...common,
      getBid: Fun([UInt], Maybe(UInt)),
      showBid: Fun([Bool, Maybe(UInt)], Null),
    }),
  ],
  (Auctioneer, Bidder) => {

    Auctioneer.only(() => {
      const [ tokA, amtA, tokB, reservePrice, timeout ] = declassify(interact.getSwap());
      assume(tokA != tokB);
    });
    Auctioneer
      .publish(tokA, amtA, tokB, reservePrice, timeout)
      .pay([ [amtA, tokA ]]);

    var [ dealMade ] = [ false ];
    invariant(balance(tokA) == (dealMade ? 0 : amtA) && balance(tokB) == 0);
    while (!dealMade) {

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

      transfer(isFirstBid ? 0 : amtA, tokA).to(highestBidder);
      transfer(isFirstBid ? 0 : currentPrice, tokB).to(Auctioneer);

      [ dealMade ] = [ !isFirstBid ];
      continue;
    }

    transfer(balance()).to(Auctioneer);
    commit();
  }
);
