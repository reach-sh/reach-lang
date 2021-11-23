'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
  });
  const U = API('Writer', {
    f: Fun([], Null),
  });
  deploy();
  A.publish();

  const x =
    parallelReduce(0)
    .invariant(balance() == 0)
    .while( x < 10 )
    .api(U.f, (k) => {
        k(null);
        return x + 1;
    })
    .api(U.f, (k) => {
        k(null);
        return x + 2;
    });

  commit();
  exit();
});
