'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    stop: Fun([], Bool),
  });
  deploy();

  A.publish();

  const keepGoing =
    parallelReduce(true)
    .while(keepGoing)
    .invariant(balance() == 0)
    .case(A,
      () => ({ when: declassify(interact.stop()) }),
      () => { return [ false ]; })
    .timeout(relativeSecs(100), () => {
      return [ false ];
    });

  commit();
});
