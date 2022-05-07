'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const D = Participant('Admin', {
    price: UInt,
    deadline: UInt,
    ready: Fun([], Null),
  });
  const A = API('Attendee', {
    iWillGo: Fun([], Bool),
  });
  const C = API('Checkin', {
    theyCame: Fun([Address], Bool),
    timesUp: Fun([], Bool),
  });
  init();

  D.only(() => {
    const price = declassify(interact.price);
    const deadline = declassify(interact.deadline);
  });
  D.publish(price, deadline);
  commit();
  D.publish();
  D.interact.ready();

  const deadlineBlock = relativeTime(deadline);
  const RSVPs = new Set();

  const [ keepGoing, howMany ] =
    parallelReduce([true, 0])
    .invariant(
      balance() == howMany * price
      && RSVPs.Map.size() == howMany
    )
    .while( keepGoing )
    .api_(A.iWillGo,
      () => {
        const who = this;
        check( ! RSVPs.member(who), "not yet" );
        return [ price, (k) => {
          k(true);
          RSVPs.insert(who);
          return [ keepGoing, howMany + 1 ];
        }];
      })
    .api_(C.theyCame,
      (who) => {
        check( this == D, "you are the boss");
        check( RSVPs.member(who), "yep" );
        return [(k) => {
          k(true);
          transfer(price).to(who);
          RSVPs.remove(who);
          return [ keepGoing, howMany - 1 ];
        }];
      })
    .timeout( deadlineBlock, () => {
      const [ [], k ] = call(C.timesUp);
      k(true);
      return [ false, howMany ]
    });
  const leftovers = howMany;
  transfer(leftovers * price).to(D);
  commit();
  exit();
});
