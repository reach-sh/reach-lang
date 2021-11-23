'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    ...hasConsoleLogger,
  });
  const U = API('Writer', {
    f: Refine(Fun([UInt], UInt), (([x]) => x > 0), (([x], y) => y > x)),
  });
  deploy();
  A.publish();

  const [ x ] =
    parallelReduce([ 1 ])
    .invariant(balance() == 0 && x > 0)
    .while( x < 10 )
    .api(U.f, (_) => 0, (i, k) => {
      const xp = x + i;
      k(xp);
      return [xp];
    });
  commit();

  exit();
});
