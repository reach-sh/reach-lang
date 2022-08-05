'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, y: UInt, z: UInt, show: Fun(true, Null) });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    const z = declassify(interact.z);
  });
  A.publish(x, y, z);
  const r1 = veriMuldiv(x, y, z);
  A.interact.show(r1);
  commit();
});
