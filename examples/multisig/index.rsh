'reach 0.1';
'use strict';

/*
  This is an abstraction of a multi-signature wallet.

  Once Reach has support for dynamic sets of participants and
  participant-polymorphic abstractions it will be easier to write a
  real multi-signature wallet, but for now, here is a demonstration of
  how something this would work.

  The parent deposits money into the account at the beginning and the
  child makes requests to extract the money.

  In this way, the child is like one of the peers in the wallet and
  the parent is like the set of all the other peers who determine if
  the transaction goes through.

*/

export const main =
  Reach.App(
    {},
    [Participant('Child', { request: Fun([UInt], UInt) } ),
     Participant('Parent', { allowance: Fun([], UInt),
                  approve: Fun([UInt, UInt], Bool) } )],
    (Child, Parent) => {
      Parent.publish();
      commit();

      Child.publish();
      commit();

      Parent.only(() => {
        const allowance = declassify(interact.allowance()); });
      Parent.publish(allowance)
        .pay(allowance);

      var [ bal, oks, nos ] = [ allowance, 0, 0 ];
      invariant(balance() == bal);
      while ( bal != 0 ) {
        commit();

        Child.only(() => {
          const howMuch = declassify(interact.request(bal));
          assume(howMuch <= bal); });
        Child.publish(howMuch);
        require(howMuch <= bal);
        commit();

        Parent.only(() => {
          const approval = declassify(interact.approve(howMuch, bal)); });
        Parent.publish(approval);

        if ( approval ) {
          transfer(howMuch).to(Child);
          [ bal, oks, nos ] = [ bal - howMuch, oks + 1, nos ];
          continue; }
        else {
          [ bal, oks, nos ] = [ bal, oks, nos + 1 ];
          continue; } }

      commit(); } );
