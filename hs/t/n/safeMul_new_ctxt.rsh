'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, show: Fun(true, Null) });
  setOptions({ verifyArithmetic: true });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = safeMul(x, 5);
  // ^ Having SMT assume `x * 5` is safe statically
  // does not affect usages of `x` later
  const z = x * 5;
  assert (z < UInt.max);
  A.interact.show(y);
  A.interact.show(z);
  commit();
});
