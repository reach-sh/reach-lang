'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    log: Fun(true, Null),
  });
  const B = API({
    go : Fun([UInt], Bool),
  });
  init();

  A.publish();

  const keepGoing =
    parallelReduce(true)
    .while(keepGoing)
    .invariant(balance() == 0)
    .api_(B.go, (x) => {
      return [ k => {

        const keepGoin2g =
          parallelReduce(true)
          .while(keepGoing)
          .invariant(balance() == 0)
          .api_(B.go, (x1) => {
            return [ k1 => {
              k1(false);
              return false;
            }];
          })
          .timeout(false);

        return false;
      }];
    })
    .timeout(false);

  commit();

});
