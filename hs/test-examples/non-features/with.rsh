'reach 0.1';

export const main = Reach.App(
  {},
  [['A', { get: Fun([], Object({x: UInt256, y: UInt256})),
           put: Fun([ UInt256, UInt256 ], Null) }]],
  (A) => {
    A.only(() => {
      const o = declassify(interact.get());
      with (o) {
        interact.put(x, y); } }); });
