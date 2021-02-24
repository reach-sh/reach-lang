'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('A', { get: Fun([], Array(Object({x: UInt, y: UInt}), 32)),
           put: Fun([ Array(UInt, 12),
                      UInt, UInt ], Null) })],
  (A) => {
    const a0 =
          Array.iota(5)
          .concat(Array.iota(5))
          .concat(Array.empty)
          .concat(Array.replicate(2, 1));
    const a1 = a0.map(x => x+1);
    const v2 = a1.reduce(0, add);

    A.only(() => {
      const a3 = declassify(interact.get());
      const a4 =
            Array.iota(35)
            .zip(
              a3.map(o => o.x)
                .concat(Array.iota(3)
                        .map(x => x+1)))
            .map(x => x[0] + x[1]);
      const v5 = a4.reduce(v2, mul);
      const a6 =
            Array.map(Array.iota(4),
                      Array.iota(4),
                      Array.iota(4),
                      (x, y, z) => x + y + z);
      const a7 =
            Array.reduce(
              Array.iota(4),
              Array.iota(4),
              Array.iota(4),
              0, (acc, x, y, z) => acc + x + y + z); });

    assert(
      Array
        .iota(4)
        .reduce(Array.iota(4), 0, (x, y, z) => (z + x + y)) ==
        ((((0 + 0 + 0) + 1 + 1) + 2 + 2) + 3 + 3));

    A.publish(a3, a4, v5, a6, a7);
    const c4 =
          Array.map(Array.iota(35),
                    a3.map(o => o.x)
                    .concat(Array.iota(3)
                            .map(x => x+1)),
                   add);
    const c5 = c4.reduce(v5, mul);
    commit();

    A.only(() => {
      const x = c5; });
    A.publish(x);
    require(x == c5);
    commit();

    A.only(() => {
      interact.put(a1, v2, c5); });

    exit();
  });
