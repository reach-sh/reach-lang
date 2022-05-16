'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    d: Digest,
    expected: UInt,
  });
  init();
  A.only(() => {
    const d = declassify(interact.d);
    const e = declassify(interact.expected);
    check(e == d % 4);
  });
  A.publish(d, e);
  check(e == d % 4);
  commit();
});
