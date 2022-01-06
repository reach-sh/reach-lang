'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  const [x] = parallelReduce([0])
    .while(true)
    .invariant(balance() == 0);
  commit();
});
