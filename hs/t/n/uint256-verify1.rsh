'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ verifyArithmetic: true });
  const A = Participant('A', {
    x: UInt256,
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = UInt(x);
  });
  A.publish(y);
  commit();
  exit();
});

