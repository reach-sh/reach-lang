'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, y: UInt, z: UInt, show: Fun(true, Null) });
  setOptions({ verifyArithmetic: true });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    const z = declassify(interact.z);
  });
  A.publish(x, y, z);
  const r1 = safeMuldiv(x, y, z);
  enforce(z != 0);
  const r2 = muldiv(x, y, z);
  A.interact.show(r1);
  A.interact.show(r2);
  commit();
});
