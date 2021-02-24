'reach 0.1';

export const main = Reach.App(
  {},
  [Participant('A', { get: Fun([], Object({x: UInt, y: UInt})),
           put: Fun([ UInt, UInt ], Null) })],
  (A) => {
    A.only(() => {
      const o = declassify(interact.get());
      with (o) {
        interact.put(x, y); } }); });
