'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const [ x ] =
        parallel_reduce([ 0 ])
          .invariant(balance() == balance())
          .while(true)
          .case(A,
            (() => { when: true }),
            (() => {
              return [ x + 1]
            })
          )
         .time_remaining(1, () => {});
    });
