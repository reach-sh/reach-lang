'reach 0.1';

const MUI = Maybe(UInt);
export const main = Reach.App(() => {
  const A = Participant('A', {
    who1: Address,
    who2: Address,
  });
  init();
  A.only(() => {
    const i = 1;
    const who1 = declassify(interact.who1);
    const who2 = declassify(interact.who2);
  });
  A.publish(i, who1, who2);
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

  const wx1 = M[who1];
  const wy1 = M[who1];
  assert(wy1 == wx1);
  const wxs1 = fromSome(wx1, 15);
  const wys1 = fromSome(wy1, 15);
  require(wxs1 == wys1);

  const wx2 = M[who2];
  const wy2 = M[who2];
  assert(wy2 == wx2);
  const wxs2 = fromSome(wx2, 25);
  const wys2 = fromSome(wy2, 25);
  require(wxs2 == wys2);

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
    const [ who3, xsa, ysa, va, xa, ya, za ] = [ who2, xs, ys, v, x, y, z ];
  });
  A.publish(who3, xsa, ysa, va, xa, ya, za);
  require(wxs2 == fromSome(M[who3], 25));
  require(xsa == xs);
  require(ysa == ys);
  require(va == v);
  require(xa == x);
  require(ya == y);
  require(za == z);
  delete M[A];
  const u = M[A];
  assert(u == MUI.None());
  commit();
  exit();
});
