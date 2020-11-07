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
     ['Player',
      { ...Common,
        confirmTicket: Fun([UInt], Null) },
      'class' ],
     ['Winner', {}],
    ],
    (Sponsor, Player, Winner) => {
      Sponsor.only(() => {
        const { ticketPrice, deadline } =
          declassify(interact.getParams());
      });
      Sponsor.publish(ticketPrice, deadline);
      commit();

      var [ boughtM, howMany ] = [ new Set(), 0 ];
      invariant(balance() = howMany * ticketPrice);
      while ( forEach(Player).until(deadline) ) {
        // The body must always be an `only`
        Player.only(() => {
          interact.confirmTicket(ticketPrice);
          const _ticket = interact.random();
          const ticketCommit = declassify(digest(_ticket));
        });
        // Then a publish and/or pay
        Player.publish(ticketCommit)
          .pay(ticketPrice);

        [ boughtM, howMany ] = [ boughtM.add(Player), howMany + 1 ];
        continue;
      }
      commit();

      var [ ticketsM, hwinner, howManyReturned ] = [ new Map(UInt), 0, 0 ];
      invariant(balance() = howMany * ticketPrice);
      while ( forEach(Player).until(deadline) ) {
        Player.only(() => {
          const ticket = declassify(_ticket);
          assume(Player.bought == true);
        });
        Player.publish(ticket);
        require(bought.member(Player));
        require(ticket == commit(ticketCommit));

        [ ticketsM, hwinner, howManyReturned ] =
          [ ticketsM.set(Player, howManyReturned),
            (hwinner + (ticket % howMany)) % howMany,
            howManyReturned + 1 ];
        continue;
      }
      commit();

      const howManyNotReturned = howMany - howManyReturned;
      const winner = hwinner % howManyReturned;
      each([Sponsor, Player], () => {
        interact.showWinner(winner);
      });

      const isWinner = (who) => {
        // The mapping associated with the class is available for inspection
        const tn = ticketsM[who];
        switch ( tn ) {
          case None: return false;
          case Some: return tn == winner;
        }
      };

      Winner.only(() => {
        assume(isWinner(Winner));
      });
      Winner.publish();
      require(isWinner(Winner));
      transfer(howManyReturned * ticketPrice).to(Winner);
      transfer(howManyNotReturned * ticketPrice).to(Sponsor);
      commit();
    });
