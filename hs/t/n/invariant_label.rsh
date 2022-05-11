'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();

  const keepGoing =
    parallelReduce(true)
    .while(true)
    .invariant(balance() == 0, "GABBAGOOL")
    .case(A,
      () => ({ when: true }),
      (_) => 1,
      (_) => { return false; }
    )
    .timeout(relativeSecs(10), () => {
      Anybody.publish();
      return false;
    });

  commit();
})
