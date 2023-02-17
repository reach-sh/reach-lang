'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const D = Participant('D', {
    ready: Fun([], Null),
  });
  const E = Events({
    e: [UInt],
  });
  const A = API({
    poke: Fun([], Null),
  })
  init();

  D.publish();
  D.interact.ready();

  const i = parallelReduce(0)
        .invariant(balance() === 0)
        .while(true)
        .api_(A.poke, () => {
          return [0, (k) => {
            E.e(i);
            E.e(i + 1);
            k(null);
            return i + 2;
          },];
        })
  commit();
});
