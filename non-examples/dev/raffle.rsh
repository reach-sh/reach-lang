'reach 0.1';

const Common = {
  ...hasRandom,
  showWinner: Fun([UInt], Null)
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Sponsor',
      { ...Common,
        getParams: Fun([], Object({ ticketPrice: UInt,
                                    deadline: UInt })) }],
     ['class', 'Player',
      { ...Common,
        confirmTicket: Fun([UInt], Null) } ],
    ],
    (Sponsor, Player) => {
      Sponsor.only(() => {
        const { ticketPrice, deadline } =
          declassify(interact.getParams());
        const _sponsort = interact.random();
        const sponsortc = declassify(digest(_sponsort));
      });
      Sponsor.publish(ticketPrice, deadline, sponsortc);
      commit();

      const [ randomsM, howMany ] =
        parallel_reduce(
          [ new Map(Digest), 0 ],
          invariant(balance() == howMany * ticketPrice),
          until(false),
          timeout(deadline),
          [
            [ Player,
              () => {
                Player.only(() => {
                  interact.confirmTicket(ticketPrice);
                  const _ticket = interact.random();
                  const ticketCommit = declassify(digest(_ticket));
                });
                Player.publish(ticketCommit)
                  .pay(ticketPrice);

                return [ randomsM.set(Player, ticketCommit),
                         howMany + 1 ];
              } ]
          ]);
      commit();

      const randomMatches = (who, r) => {
        const rc = randomsM[who];
        switch ( rc ) {
          case None: return false;
          case Some: return rc == digest(r);
        }
      };

      const [ ticketsM, hwinner, howManyReturned ] =
        parallel_reduce(
          [ new Map(UInt), 0, 0 ],
          invariant(balance() == howMany * ticketPrice),
          until(howManyReturned == howMany),
          timeout(deadline),
          [
            [ Player,
              (_ticket, ticketCommit) => {
                Player.only(() => {
                  const ticket = declassify(_ticket);
                  assume(fromMaybe(ticketsM[Player], () => true, (_) => false);
                  assume(randomMatches(Player, ticket));
                });
                Player.publish(ticket);
                require(randomMatches(Player, ticket));

                return [ ticketsM.set(Player, howManyReturned),
                         (hwinner + (ticket % howMany)) % howMany,
                         howManyReturned + 1 ];
              } ]
          ]);
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
      const winner = (hwinner + (sponsort % howManyReturned)) % howManyReturned;
      each([Sponsor, Player], () => {
        interact.showWinner(winner);
      });

      const isWinner = (who) => {
        const tn = ticketsM[who];
        switch ( tn ) {
          case None: return false;
          case Some: return tn == winner;
        }
      };

      fork([
        [ Player,
          () => {
            Player.only(() => {
              assume(isWinner(Player)); });
            Player.publish();
            require(isWinner(Player));
            transfer(howMany * ticketPrice).to(Winner);
            commit();
          } ]
      ]);
    });
