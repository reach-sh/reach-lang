'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [ Participant('Creator', {
      benefactor: Address,
    }),
    ParticipantClass('Owner', {
      claim: Fun([Address, UInt], UInt),
    }),
  ],
  (Creator, Owner) => {
    Creator.only(() => {
      const benefactor = declassify(interact.benefactor); });
    Creator.publish(benefactor);

    var [ owner, lastBoon ] = [ Creator, 0 ];
    invariant(balance() == 0);
    while ( true ) {
      commit();
      Owner.only(() => {
        const myBoon = declassify(interact.claim(owner, lastBoon));
        const shouldClaim = myBoon > lastBoon; });
      Owner.publish(myBoon)
        .pay(myBoon)
        .when(shouldClaim)
        .timeout(false);
      require(myBoon > lastBoon);
      transfer(myBoon).to(benefactor);
      [ owner, lastBoon ] = [ this, myBoon ];
      continue;
    }

    commit();
    assert(false);
    exit();
  }
);
