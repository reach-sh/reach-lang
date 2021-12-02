'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getX: Fun([], UInt),
  });
  const E = Events('x_event', {
    x: [UInt],
    y: [UInt, UInt],
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
    E.x(4);
    E.y(x, x);
    E.y(x, 2);

    [ xl ] = [ x ];
    continue;
  }

  commit();

});
