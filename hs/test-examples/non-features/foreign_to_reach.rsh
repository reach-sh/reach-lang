'reach 0.1';

/*
  Use `exposeAs` value to give a `publish` an externally visible name
*/

export const main =
  Reach.App(
    {},
    [['A', { onStop: Fun([], Null) }]],
    (A) => {
      A.publish().pay(1).exposeAs("initialPay");

      const [ keepGoing ] =
        parallel_reduce([ true ])
          .while(keepGoing)
          .invariant(true)
          .case(A,
            (() => ({ exposeAs: "stop" })),
            (() => 1),
            (() => {
              A.only(() => interact.onStop());
              return [ false ]; })
          )
          .timeout(100, () => {
            A.publish();
            return [ false ];
          });

      transfer(balance()).to(A);
      commit();
      exit();
    }
  );
