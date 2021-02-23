'reach 0.1';

const Common = {
  showWinner: Fun([Bool, Address, UInt], Null),
};

export const main =
  Reach.App(
    {},
    [['Sponsor',
      { ...Common,
        getParams: Fun([], Object({ prize: UInt,
                                    deadline: UInt })),
      }],
     ['class', 'Bidder',
      { ...Common,
        getBid: Fun([UInt, UInt, UInt], UInt),
      } ],
    ],
    (Sponsor, Bidder) => {
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
        parallel_reduce([ Sponsor, 0 ])
        .invariant(balance() == prize)
        .while( keepBidding() )
        .case( Bidder, (() => {
            const previousBid = getBid(this);
            const addl =
              declassify(interact.getBid(prize, winningBid, previousBid));
            const newBid = previousBid + addl;
            return { msg: addl, when: addl != 0 };
          }),
          ((addl) => addl),
          ((addl) => {
            transfer(addl).to(Sponsor);
            const newBid = getBid(this) + addl;
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
        .timeout(bidTimeout(), () => {
          race(Sponsor, Bidder).publish();
          return [ winner, winningBid ]; });
      commit();

      Bidder.only(() => {
        const itsame = winner == this; });
      Bidder.publish().when(itsame)
        .timeout(deadline, () => closeTo(Sponsor, () => {}));
      transfer(prize).to(winner);
      commit();

      each([Sponsor, Bidder], () => {
        interact.showWinner(true, winner, winningBid);
      });

    });
