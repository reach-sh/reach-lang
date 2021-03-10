'reach 0.1';

export const main =
  Reach.App(
    {},
    [ Participant('Creator', {
        getId: Fun([], UInt),
      }),
      ParticipantClass('Owner', {
        newOwner: Fun([], Address),
        showOwner: Fun([UInt, Address], Null),
      })
    ],
    (Creator, Owner) => {
      Creator.only(() => {
        const id = declassify(interact.getId()); });
      Creator.publish(id);

      var owner = Creator;
      invariant(balance() == 0);
      while ( true ) {
        commit();

        Owner.only(() => {
          interact.showOwner(id, owner);
          const amOwner = this == owner;
          const newOwner =
            amOwner ? declassify(interact.newOwner()) : this; });
        Owner.publish(newOwner)
          .when(amOwner)
          .timeout(false);
        require(this == owner);
        // We could add a pay above and transfer(fee).to(Creator) below to
        // implement a basic royalty.
        //
        // Rather than the owner just doing a transfer, we could have the owner
        // start an auction, and encode the logic of the auction here and the
        // creator could get a cut.
        //
        // We could change from a straight transfer to the creator and instead
        // manage a royalty fund with its own shares
        owner = newOwner;
        continue;
      }
      commit();

      assert(false);
    });
