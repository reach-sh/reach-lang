'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getX: Fun([], UInt),
    checkX: Fun(true, Null),
    checkY: Fun(true, Null)
  });
  const E = Events('x_event', {
    x: [UInt],
    y: [UInt, UInt],
  });
  deploy();

  A.publish();

  var [ xl ] = [ 0 ];
  invariant(balance() == 0);
  while (xl < 5) {
    commit();
    A.only(() => {
      const x = declassify(interact.getX());
    });
    A.publish(x);

    E.x(x);
    A.interact.checkX(false);

    E.x(4);
    A.interact.checkX(true);

    E.y(x, x);
    A.interact.checkY(false);

    E.y(x, 2);
    A.interact.checkY(true);

    [ xl ] = [ x ];
    continue;
  }

  commit();

});
