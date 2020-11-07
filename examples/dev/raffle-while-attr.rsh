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
       // This indicates that Player is a "class", which means it has a mapping
       // associated with it, and must be used with forEach
      'class',
       { ticket: MUInt,
         bought: Bool }],
     ['Winner', {}],
    ],
    (Sponsor, Player, Winner) => {
      Sponsor.only(() => {
        const { ticketPrice, deadline } =
          declassify(interact.getParams());
      });
      Sponsor.publish(ticketPrice, deadline);
      commit();

      // This `while` `forEach` means it is all in parallel
      var howMany = 0;
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

        // It must end by setting all the components of the mapping that are not
        // already set
        Player.ticketNumber = MUInt.None();
        Player.bought = true;
        // As well as the while variables
        howMany = howMany + 1;
        continue;
      }
      commit();

      var [ hwinner, howManyReturned ] = [ 0, 0 ];
      invariant(balance() = howMany * ticketPrice);
      while ( forEach(Player).until(deadline) ) {
        Player.only(() => {
          const ticket = declassify(_ticket);
          assume(Player.bought == true);
        });
        Player.publish(ticket);
        require(Player.bought == true);
        require(ticket == commit(ticketCommit));

        Player.ticket = MUInt.Some(howManyReturned);
        [ hwinner, howManyReturned ] =
          [ (hwinner + (ticket % howMany)) % howMany,
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
        const tn = Player[who].ticket;
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
