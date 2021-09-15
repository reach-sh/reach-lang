'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    check: Fun(true, Null),
    x: UInt,
    y: UInt,
    z: UInt,
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    const z = declassify(interact.z);
    const r1 = muldiv(x, y, z);
  });
  A.publish(x, y, z, r1);

  const r2 = muldiv(x, y, z);

  require(r1 == r2);

  commit();

  const a = muldiv(x, y, z);
  A.interact.check(a);

});
