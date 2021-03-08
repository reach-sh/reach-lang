'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.publish();

      const [ x ] =
        parallel_reduce([ 0 ])
          .invariant(balance() == balance())
          .while(true)
          .case(A,
            (() => ({
              when: true })),
            (() => {
              return [x + 1];}))
          .timeout(1, () => {
            Anybody.publish();
            return [ x ]; });

      transfer(balance()).to(A);
      commit();
    });
