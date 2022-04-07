'reach 0.1';

const m = UInt256(18446744073709551616);

export const main = Reach.App(() => {
  setOptions({ verifyArithmetic: true });
  const A = Participant('A', {
    x: UInt256,
  });
  init();
  const f = (x) => {
    check(x > m);
    check(x < UInt256(2)*m);
  };
  A.only(() => {
    const x = declassify(interact.x);
    f(x);
  });
  A.publish(x);
  f(x);
  const y = x + UInt256(1);
  commit();
  exit();
});
