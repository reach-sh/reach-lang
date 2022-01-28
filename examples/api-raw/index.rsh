'reach 0.1';
'use strict';

const B13 = Array(Bool, 13);
const B7 = Array(Bool, 7);
const BT8 = Tuple(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool);
export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    ready: Fun([], Null),
    check: Fun(true, Null),
  });
  const V = View('V', {
    v: Fun([UInt, UInt], UInt),
  });
  const P = API('P', {
    f: Fun([UInt, UInt], Bool),
    g: Fun([UInt], UInt),
    h1: Fun([UInt, B13], Tuple(Bool, UInt, B13)),
    h2: Fun([UInt, B7, BT8], Tuple(Bool, UInt, B7, BT8)),
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  V.v.set((a, b) => a + b + x);
  commit();
  A.interact.ready();

  fork()
    .api(P.f, ((y, _) => { assume(y == x); }), ((y, _) => y),
      (y, z, k) => {
        require(y == x);
        k(true);
        A.interact.check([x, y, z]);
      })
    .api(P.g, ((y) => { assume(y == x); }), ((y) => y),
      (y, k) => {
        require(y == x);
        k(x + y);
        A.interact.check([x, y]);
      })
    .api(P.h1, ((y, b13) => {
      assume(y == x);
      assume(b13[1]);
      assume(b13[12]);
    }), ((y, _) => y),
      (y, b13, k) => {
        require(y == x);
        require(b13[1]);
        require(b13[12]);
        k([true, 7, b13]);
        A.interact.check([x, y, b13]);
      })
    .api(P.h2, ((y, b7, b8) => {
      assume(y == x);
      assume(b7[1]);
      assume(b8[4]);
    }), ((y, _, _) => y),
      (y, b7, b8, k) => {
        require(y == x);
        require(b7[1]);
        require(b8[4]);
        k([true, 7, b7, b8]);
        A.interact.check([x, y, b7, b8]);
      });

  commit();
  A.publish();
  transfer(x).to(A);
  commit();

  exit();
});
