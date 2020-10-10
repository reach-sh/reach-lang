'reach 0.1';

export const main = Reach.App(
  {},
  [['A', { put: Fun([UInt], UInt) }]],
  (A) => {
    A.only(() => {
      const x = -1;
    });
    A.publish(x);
    const y = -1;
    const z = x + y;
    commit();

    A.only(() => {
      const a = declassify(interact.put(z));
      assume(a == z);
    });
    A.publish(a);
    require(a == z);
    commit();

    exit();
  });

