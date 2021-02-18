'reach 0.1';

const Common = {
  ...hasRandom,
  showWinner: Fun([UInt], Null),
  showOutcome: Fun([Address], Null),
};

export const main =
  Reach.App(
    { /*connectors: [ ETH ]*/ },
    [['Sponsor',
      { ...Common,
        getParams: Fun([], Object({ ticketPrice: UInt,
                                    deadline: UInt })) }],
     ['class', 'Player',
      { ...Common,
        shouldBuy: Fun([UInt, UInt], Bool),
        buyerWas: Fun([Address], Null),
        returnerWas: Fun([Address, UInt], Null),
        recoverTicket: Fun([], UInt) } ],
    ],
    (Sponsor, Player) => {
      Sponsor.only(() => {
        const { ticketPrice, deadline } =
          declassify(interact.getParams());
        const _sponsort = interact.random();
        const sponsortc = declassify(digest(_sponsort));
      });
      Sponsor.publish(ticketPrice, deadline, sponsortc);

      const endTime_buy = lastConsensusTime() + deadline;
      const endTime_ret = endTime_buy + deadline;
      const [ isTime, BUYING, RETURNING ] = makeEnum(2);
      const endTime = [ endTime_buy, endTime_ret ];
      const timeRemaining = (which) =>
        (endTime[which] - lastConsensusTime());
      const keepGoing = (which) =>
        (endTime[which] > lastConsensusTime());

      const randomsM = new Map(Digest);
      const [ howMany ] =
        parallel_reduce([ 0 ])
        .invariant(balance() == ticketPrice * howMany)
        .while( keepGoing(BUYING) )
        .case( Player, (() => {
            const _ticket = interact.random();
            const when = declassify(interact.shouldBuy(ticketPrice, _ticket));
            assume(implies(when, isNone(randomsM[this])));
            const msg = declassify(digest(_ticket));
            return { msg, when };
          }),
          (() => ticketPrice),
          ((ticketCommit) => {
            const player = this;
            require(isNone(randomsM[player]));
            Player.only(() => interact.buyerWas(player));
            randomsM[player] = ticketCommit;
            return [ howMany + 1 ];
          })
        )
        // XXX Add a short-hand for timeouts like this
        .timeout(timeRemaining(BUYING), () => {
          race(Sponsor, Player).publish();
          return [ howMany ]; });

      const randomMatches = (who, r) => {
        const rc = randomsM[who];
        switch ( rc ) {
          case None: return false;
          case Some: return rc == digest(r);
        }
      };

      const ticketsM = new Map(UInt);
      const [ hwinner, howManyReturned ] =
        parallel_reduce([ 0, 0 ])
        .invariant(balance() == howMany * ticketPrice)
        .while( keepGoing(RETURNING) && howManyReturned < howMany )
        .case( Player, (() => {
            const player = this;
            const ticket = declassify(interact.recoverTicket());
            const when = isNone(ticketsM[player])
              && randomMatches(player, ticket);
            return { msg: ticket, when };
          }),
          ((ticket) => {
            const player = this;
            Player.only(() => interact.returnerWas(player, howManyReturned));
            require(isNone(ticketsM[player]));
            require(randomMatches(player, ticket));
            ticketsM[player] = howManyReturned;
            delete randomsM[player];
            return [ (hwinner + (ticket % howMany)) % howMany,
                     howManyReturned + 1 ];
          })
        )
        .timeout(timeRemaining(RETURNING), () => {
          race(Sponsor, Player).publish();
          return [ hwinner, howManyReturned ]; });
      commit();

      // Here's an attack:
      // 1. Know that you are the last one to return
      // 2. Compute who the winner is if you submit your ticket
      // 3. Blackmail them to pay you to submit your ticket
      //
      // Here's another attack:
      // 1. Know you are the current winner
      // 2. Buy all the blocks until the deadline, so other people can't submit
      // their ticket.
      //
      // The sponsor's randomness attempts to compensate for these attacks by
      // making it so that they have to contribute this. The best case scenario
      // for the sponsor to control the outcome is to know what their ticket is
      // and ALSO submit a bunch of other tickets, which would be too costly.
      Sponsor.only(() => {
        const sponsort = declassify(_sponsort); });
      Sponsor.publish(sponsort);
      require(sponsortc == digest(sponsort));

      const howManyNotReturned = howMany - howManyReturned;
      const winning_no = (hwinner + (sponsort % howManyReturned)) % howManyReturned;
      commit();

      each([Sponsor, Player], () => {
        interact.showWinner(winning_no);
      });

      const isWinner = (who) => {
        const tn = ticketsM[who];
        switch ( tn ) {
          case None: return false;
          case Some: return tn == winning_no;
        }
      };

      fork()
      .case(Player, (() => ({
          when: isWinner(this),
        })),
        (() => {
          const winner = this;
          require(isWinner(winner));
          each([Sponsor, Player], () => {
            interact.showOutcome(winner);
          });
          transfer(howMany * ticketPrice).to(winner);
        }))
      .timeout(deadline, () => closeTo(Sponsor, () => {}));
      commit();
    });
