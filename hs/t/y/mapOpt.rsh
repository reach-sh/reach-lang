'reach 0.1';

const MUI = Maybe(UInt);
export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.only(() => {
    const i = 1;
  });
  A.publish(i);
  const M = new Map(UInt);
  M[A] = 0;
  commit();
  A.publish();
  const x = M[A];
  assert(x == MUI.Some(0));
  const y = M[A];
  assert(y == MUI.Some(0));
  const xs = fromSome(x, 15);
  const ys = fromSome(y, 15);
  assert(xs == ys);
  M[A] = 1;
  const z = M[A];
  assert(z == MUI.Some(1));
  if ( i < 10 ) {
    M[A] = 2;
  }
  const v = M[A];
  assert(v == MUI.Some(i < 10 ? 2 : 1));
  commit();
  A.only(() => {
    const [ va, xa, ya, za ] = [ v, x, y, z ];
  });
  A.publish(va, xa, ya, za);
  require(va == v && xa == x && ya == y && za == z);
  delete M[A];
  const u = M[A];
  assert(u == MUI.None());
  commit();
  exit();
});
