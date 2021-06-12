'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      const [ x ] =
        parallelReduce([ 0 ])
          .invariant(balance() == balance())
          .while(true)
          .case(A,
            (() => { when: true }),
            (() => {
              return [ x + 1]
            })
          )
         .timeRemaining(1, () => {});
    });
