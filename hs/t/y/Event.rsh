'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getX: Fun([], UInt),
  });
  const E = Event('x_event', {
    x: Tuple(UInt),
  });
  deploy();

  A.publish();

  var [ xl ] = [ 0 ];
  invariant(balance() == 0);
  while (xl < 10) {
    commit();
    A.only(() => {
      const x = declassify(interact.getX());
    });
    A.publish(x);
    E.x(x);

    [ xl ] = [ x ];
    continue;
  }

  commit();

});
