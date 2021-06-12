'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [ Participant('Creator', {
      benefactor: Address,
    }),
    ParticipantClass('Owner', {
      claim: Fun([Address, UInt, UInt], UInt),
    }),
  ],
  (Creator, Owner) => {
    Creator.only(() => {
      const benefactor = declassify(interact.benefactor); });
    Creator.publish(benefactor);

    const boons = new Map(UInt);
    const getBoon = (who) =>
      boons[who].match({
        None: (() => 0),
        Some: ((x) => x)
      });
    var [ owner, lastBoon ] = [ Creator, 0 ];
    invariant(balance() == 0);
    while ( true ) {
      commit();
      Owner.only(() => {
        const myBoon = getBoon(this);
        const myBoonInc = declassify(interact.claim(owner, myBoon, lastBoon));
        const myNewBoon = myBoon + myBoonInc;
        const shouldClaim = myNewBoon > lastBoon; });
      Owner.publish(myBoonInc)
        .pay(myBoonInc)
        .when(shouldClaim)
        .timeout(false);
      const thisNewBoon = getBoon(this) + myBoonInc;
      require(thisNewBoon > lastBoon);
      boons[this] = thisNewBoon;
      transfer(myBoonInc).to(benefactor);
      [ owner, lastBoon ] = [ this, thisNewBoon ];
      continue;
    }

    commit();
    assert(false);
    exit();
  }
);
