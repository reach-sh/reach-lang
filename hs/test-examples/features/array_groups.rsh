'reach 0.1';

export const main = Reach.App(
  {},
  [['A', { get: Fun([], Array(Object({x: UInt256, y: UInt256}), 32)),
           put: Fun([ Array(UInt256, 10),
                      UInt256 ], Null) }]],
  (A) => {
    const a0 = Array.iota(5).concat(Array.iota(5));
    const a1 = a0.map(x => x+1);
    const v2 = a1.reduce(0, add);

    /*A.only(() => {
      const a3 = declassify(interact.get());
      const a4 = a3.map(o => o.x).concat(Array.iota(3).map(x => x+1));
      const v5 = a4.reduce(v2, mul); });
    A.publish(a4, v5);
    commit();
    */
    A.only(() => {
      interact.put(a1, v2 /*a0, a4, v5*/); });
    
    exit();
  });
