'reach 0.1';

export const main = Reach.App(
  { verifyOverflow: true },
  [Participant('A', { get: Fun([], Array(UInt, 16)),
           put: Fun([Array(UInt, 16)], Null) })],
  (A) => {
    A.only(() => {
      const x = declassify(interact.get());
      x.forEach(xe => assume(xe < UInt.max));
    });
    A.publish(x);
    x.forEach(xe => require(xe < UInt.max));
    const y = x.map(xe => xe + 1);
    commit();

    A.only(() => {
      interact.put(y);
    });

    exit();
  });
