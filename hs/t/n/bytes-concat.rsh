'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Bytes(5),
    y: Bytes(6),
    f: Fun([Bytes(11)], Null),
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
  });
  A.publish(x, y);
  const z1 = x.concat(17);
  const z2 = Bytes.concat(x, y);
  commit();
  A.only(() => {
    const z3 = z1;
  });
  A.publish(z3);
  check(z1 == z3);
  check(z1 == z2);
  A.interact.f(z3);
  commit();
  exit();
});
