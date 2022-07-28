'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt256, show: Fun([UInt], Null) });
  setOptions({ verifyArithmetic: true });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = UInt(x, false, false);
  assert ( y <= UInt.max );
  A.interact.show(y);
  commit();
});
