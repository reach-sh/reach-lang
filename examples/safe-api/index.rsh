'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    go: Fun([], Bool),
  });
  const B = API({
    go: Fun([], Bool),
  });
  deploy();
  A.publish();

  const [ keepGoing ] =
    parallelReduce([ true ])
    .while(keepGoing)
    .invariant(balance() == 0)
    .case(A,
      (() => ({
        when: declassify(interact.go())
      })),
      ((_) => {
        return [ false ];
      })
    )
    .api(B.go,
      ((k) => {
        k(true);
        return [ true ];
      })
    )
    .timeout(false);

  commit();
  exit();
});
