'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    when: Fun([], Bool)
  });

  init();

  A.publish();

  const [ a ] =
    parallelReduce([ true ])
    .invariant(balance() == 0)
    .while(a)
    .case(A,
      () => ({
         when: declassify(interact.when()),
      }),
      (_) => {
        return [ false ];
      })
     .timeout(relativeTime(10), () => {
       Anybody.publish();
       return [ true ];
     });

  commit();

});
