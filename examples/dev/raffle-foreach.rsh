'reach 0.1';

const MUInt = Maybe(UInt);

const Common = {
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
    (Sponsor, Player, Winner) => {
      Sponsor.only(() => {
        const { ticketPrice, deadline } =
          declassify(interact.getParams());
      });
      Sponsor.publish(ticketPrice, deadline);
      commit();

      const [ randomsM, howMany ] =
        parallel_reduce(
          deadline,
          [ new Map(Digest), 0 ],
          invariant(balance() == howMany * ticketPrice),
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

      const randomMatches = (who, r) => {
        const rc = randomsM[who];
        switch ( rc ) {
          case None: return false;
          case Some: return rc == digest(r);
        }
      };

      const [ ticketsM, hwinner, howManyReturned ] =
        parallel_reduce(
          deadline,
          [ new Map(UInt), 0, 0 ],
          invariant(balance() == howMany * ticketPrice),
          [
            [ Player,
              (_ticket, ticketCommit) => {
                Player.only(() => {
                  const ticket = declassify(_ticket);
                  assume(randomMatches(Player, ticket));
                });
                Player.publish(ticket);
                require(randomMatches(Player, ticket));

                return [ ticketsM.set(Player, howManyReturned),
                         (hwinner + (ticket % howMany)) % howMany,
                         howManyReturned + 1 ];
              } ]
          ]);

      const howManyNotReturned = howMany - howManyReturned;
      const winner = hwinner % howManyReturned;
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
            transfer(howManyReturned * ticketPrice).to(Winner);
            transfer(howManyNotReturned * ticketPrice).to(Sponsor);
            commit();
          } ]
      ]);
    });
