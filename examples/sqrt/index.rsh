'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    y: UInt,
    x256: UInt256,
    y256: UInt256,
  });
  init();
  const f = (kx, ky) => {
    A.only(() => {
      const x = declassify(interact[kx]);
      const y = declassify(interact[ky]);
      const z = sqrt(x);
      check(y == z);
    });
    A.publish(x, z);
    check(z == sqrt(x));
    commit();
  };
  f('x', 'y');
  f('x256', 'y256');
  exit();
});
