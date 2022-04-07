'reach 0.1';

const MU = Maybe(UInt);

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Array(MU, 2),
    t: Tuple(UInt, UInt),
  });
  init();
  A.only(() => {
    // Introduce var that will generate same SMT type as Map(UInt, UInt)
    const x = declassify(interact.x);
    const [ a, b ] = declassify(interact.t);
  });
  A.publish(x, a, b);
  const m = new Map(UInt, UInt);
  m[0] = a;
  m[1] = b;
  check(m[a] == MU.Some(b));
  commit();

});
