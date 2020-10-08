'reach 0.1';

export const main = Reach.App(
  { verifyOverflow: true },
  [['A', { get: Fun([], UInt),
           put: Fun([UInt], Null) }]],
  (A) => {
    A.only(() => {
      const x = declassify(interact.get());
      assume(x < 1024);
    });
    A.publish(x);
    require(x < 1024);
    const y = x + 1;
    commit();

    A.only(() => {
      interact.put(y);
    });

    exit();
  });
