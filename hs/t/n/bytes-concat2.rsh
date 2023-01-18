'reach 0.1';
export const main = Reach.App(() => {
  //setOptions({ connectors: [ALGO] });
  const A = Participant('A', {
    x: Bytes(5),
    y: Bytes(6),
    f: Fun([Bytes(11)], Null),
    g: Fun([Bytes(33)], Null),
    h: Fun([Bytes(66)], Null),
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
  });
  A.publish(x, y);
  const z1 = x.concat(y);
  const z2 = Bytes.concat(x, y);
  const z1_33 = z1.concat(z1).concat(z1);
  const z2_66 = z2.concat(z2).concat(z2).concat(z1_33);
  commit();
  A.only(() => {
    const z3 = z1;
  });
  A.publish(z3);
  check(z1 == z3);
  check(z2 == z3);
  const z3_33 = z3.concat(z3).concat(z3);
  const z3_66 = z3_33.concat(z3_33);
  A.interact.f(z3);
  check(z1_33 == z3_33);
  A.interact.g(z1_33);
  check(z2_66 == z3_66);
  A.interact.h(z2_66);
  commit();
  exit();
});
