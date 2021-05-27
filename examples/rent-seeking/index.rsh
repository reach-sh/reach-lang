'reach 0.1';
'use strict';

const Common = {
  showWinner: Fun([Bool, Address, UInt], Null),
};

export const main =
  Reach.App(() => {
    const Sponsor = Participant('Sponsor',
      { ...Common,
        getParams: Fun([], Object({ prize: UInt,
                                    deadline: UInt })),
      });
    const Bidder = ParticipantClass('Bidder',
      { ...Common,
        getBid: Fun([UInt, UInt, UInt], UInt),
      });
    deploy();

    Sponsor.only(() => {
      const { prize, deadline } =
        declassify(interact.getParams());
    });
    Sponsor.publish(prize, deadline)
      .pay(prize);

    const [ bidTimeout, keepBidding ] =
      makeDeadline(deadline);

    const bidsM = new Map(UInt);
    const getBid = (who) =>
      fromMaybe(bidsM[who], (() => 0), (x => x));

    const [ winner, winningBid ] =
      parallelReduce([ Sponsor, 0 ])
      .invariant( balance() == prize + bidsM.sum()
        && balance() == prize + Map.sum(bidsM) )
      .while( keepBidding() )
      .case( Bidder, (() => {
          const previousBid = getBid(this);
          const addl =
            declassify(interact.getBid(prize, winningBid, previousBid));
          return { msg: addl, when: addl != 0 };
        }),
        ((addl) => addl),
        ((addl) => {
          // This is here to stress the verifier
          const x = getBid(this)
          delete bidsM[this];
          bidsM[this] = x;

          const newBid = getBid(this) + addl;
          bidsM[this] = newBid;
          // This is here to stress the verifier
          bidsM[this] = newBid;
          if ( winningBid <= newBid ) { // <- <= on purpose
            each([Sponsor, Bidder], () => {
              interact.showWinner(false, this, newBid);
            });
            return [ this, newBid ];
          } else {
            return [ winner, winningBid ];
          }
        })
      )
      .timeRemaining(bidTimeout());
    commit();

    Bidder.only(() => {
      const itsame = winner == this; });
    Bidder.publish().when(itsame)
      .timeout(deadline, () => closeTo(Sponsor, () => {}));
    transfer(prize).to(winner);
    transfer(balance()).to(Sponsor);
    commit();

    each([Sponsor, Bidder], () => {
      interact.showWinner(true, winner, winningBid);
    });
  });
