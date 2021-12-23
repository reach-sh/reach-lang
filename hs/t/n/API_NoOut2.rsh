'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const C = API('C', {
    f: Fun([], Bool)
  });
  init();
  A.publish();
  const [] = parallelReduce([])
  .invariant(balance() == 0)
  .while(true)
  .api( C.f, (apiReturn) => {
      var keepGoing = true;
      invariant(balance() == 0);
      while (keepGoing) {
        commit();
        A.publish();
        keepGoing = false;
        continue;
      }
      apiReturn(true);
      return [];
  })
  .timeout(false);
  commit();
  exit();
});
