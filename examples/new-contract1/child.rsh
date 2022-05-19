'reach 0.1';

export const main = Reach.App(() => {
  const D = Participant('D', {
    y: UInt,
  });
  const P = API({
    f: Fun([UInt], UInt),
  });
  init();
  D.only(() => {
    const y = declassify(interact.y);
  });
  D.publish(y);

  const x = parallelReduce(0)
    .invariant(balance() == 0)
    .while(x < 3)
    .api_(P.f, (z) => {
      return [ 0, (k) => {
        k(x + y + z);
        return x + 1;
      }];
    })
    .timeout(false);
  commit();

  exit();
});
