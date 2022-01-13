'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    ready: Fun([], Null),
    check: Fun(true, Null),
  });
  const P = API('P', {
    f: Fun([UInt, UInt], Bool),
    g: Fun([UInt], UInt),
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  commit();
  A.interact.ready();

  fork()
    .api(P.f, ((y, _) => { assume(y == x); }), ((y, _) => y),
      (y, z, k) => {
        require(y == x);
        k(true);
        A.interact.check([x, y, z]);
      })
    .api(P.g, ((y) => { assume(y == x); }), ((y) => y),
      (y, k) => {
        require(y == x);
        k(x + y);
        A.interact.check([x, y]);
      });

  commit();
  A.publish();
  transfer(x).to(A);
  commit();

  exit();
});
