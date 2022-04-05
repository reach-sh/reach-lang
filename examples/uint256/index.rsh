'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    vs1: Tuple(UInt256, UInt256, UInt, UInt),
    check: Fun(true, Null),
  });
  init();

  A.only(() => {
    const vs1 = declassify(interact.vs1);
  });
  A.publish(vs1);
  const [b1, b2, s1, s2] = vs1;

  const b3 = UInt256(1);
  const s3 = 1;
  const s1b = UInt256(s1);
  const s2b = UInt256(s2);
  const b2s = UInt(b2);

  const f = (isN) => (g) => [
    g(b1, b2),
    g(b1, b3),
    g(b2, b3),
    g(s1, s2),
    g(s1, s3),
    g(s1, b2s),
    g(s2, s3),
    g(s1b, s2b),
    g(s1b, b3),
    ...(isN ? [
      UInt(g(s1b, s2b)),
      UInt(g(s1b, b3)),
    ] : []),
  ];
  const g = f(false);
  const h = f(true);
  commit();

  const F = (mk) => {
    A.publish();
    const x1 = mk();
    commit();
    A.only(() => {
      const x2 = x1;
    });
    A.publish(x2);
    check(x2 == x1);
    commit();
    A.interact.check(x1);
  };

  F(() => [
    h((u, v) => u + v),
    h((u, v) => u - v),
    h((u, v) => u * v),
  ]);
  F(() => [
    h((u, v) => u / v),
    h((u, v) => u % v),
    h((u, v) => u ^ v),
  ]);
  F(() => [
    g((u, v) => u < v),
    g((u, v) => u <= v),
    g((u, v) => u == v),
    g((u, v) => u >= v),
    g((u, v) => u > v),
  ]);

  exit();
});
