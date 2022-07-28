'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, show: Fun(true, Null) });
  setOptions({ verifyArithmetic: true });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = safeMod(5, x);
  enforce(x != 0);
  const z = 5 % x;
  A.interact.show(y);
  A.interact.show(z);
  commit();
});
