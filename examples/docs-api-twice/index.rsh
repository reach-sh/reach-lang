'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    deployed: Fun(true, Null),
    n: UInt,
  });
  const U = API('Writer', {
    f: Fun([], UInt),
  });
  init();
  A.only(() => {
    const n = declassify(interact.n);
  });
  A.publish(n);
  A.interact.deployed();

  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while( x < n )
    .api(U.f, (k) => {
        k(1);
        return x + 1;
    });
  const y =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while( y < n )
    .api(U.f, (k) => {
        k(2);
        return y + 1;
    });

  commit();
  exit();
});
