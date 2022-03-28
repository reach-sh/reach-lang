'reach 0.1';

const B = Bytes(36);

export const main = Reach.App(() => {
  const A = Participant('A', {
    b: B,
    expected: UInt,
  });
  init();
  A.only(() => {
    const b = declassify(interact.b);
    const exp = declassify(interact.expected);
    check(exp == b % 4);
  });
  A.publish(b, exp);
  check(exp == b % 4);
  commit();
});
