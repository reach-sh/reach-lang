'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API('B', {
    go: Fun([UInt], UInt)
  });
  setOptions({ verifyArithmetic: true });
  init();

  A.publish();

  const [ keepGoing ] =
    parallelReduce([ true ])
      .invariant(true)
      .while(keepGoing)
      .api_(B.go,
        (x) => {
          check(UInt.max - x > 5);
          return [ x, (k) => {
            k(x + 5);
            return [ false ];
          }]
        })
      .timeout(false);

  transfer(balance()).to(A);
  commit();
});
