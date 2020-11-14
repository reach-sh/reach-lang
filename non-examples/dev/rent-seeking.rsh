'reach 0.1';

const Common = {
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Funder',
      { ...Common,
        getParams: Fun([], Object({ prize: UInt,
                                    deadline: UInt })) }],
     ['class', 'Seeker',
      { ...Common,
        getBid: Fun([UInt, UInt, UInt], UInt) } ],
    ],
    (Funder, Seeker) => {
      Sponsor.only(() => {
        const { prize, deadline } =
          declassify(interact.getParams());
      });
      Sponsor.publish(prize, deadline)
        .pay(prize);
      commit();

      const readBid = (bidsM, who) =>
        bidsM[who].match({
          None: () => 0,
          Some: (x) => x });

      const [ winner, winningBid, bidsM ] =
        parallel_reduce([ Sponsor, 0, new Map(UInt) ])
          .invariant(balance() == prize &&
                     readBid(bidsM, winner) == winningBid),
          .timeout(deadline, [Sponsor, Seeker])
          .case(Seeker,
            () => {
              Seeker.only(() => {
                const bidIncrease =
                  declassify(interact.getBid(prize, winningBid,
                                             readBid(bidsM, Seeker))); });
              Seeker.publish(bidIncrease)
                .pay(bidIncrease);
              transfer(bidIncrease).to(Sponsor);
              const oldBid = readBid(bidsM, Seeker);
              const newBid = oldBid + bidIncrease;
              const newBidsM = bidsM.set(Seeker, newBid);
              const newWinningBid =
                (newBid > winningBid) ? newBid : winningBid;
              const newWinner =
                (newBid == newWinningBid) ? Seeker : winner;
              return [ newWinner, newWinningBid, newBidsM ]; },
            { repeat: true });
      transfer(prize).to(winner);
      commit();
    });
