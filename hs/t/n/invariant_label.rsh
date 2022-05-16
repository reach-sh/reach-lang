'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();

  const [keepGoing, x] =
    parallelReduce([true, 0])
    .while(true)
    .invariant(balance() == 0, "balance() == 0")
    .invariant(x == 0, "x stays zero")
    .case(A,
      () => ({ when: true }),
      (_) => 1,
      (_) => {
        return [ false, 4 ];
      }
    )
    .timeout(10, () => {
      Anybody.publish();
      return [ false, x ];
    });

  commit();
})
