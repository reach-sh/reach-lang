'reach 0.1';

// https://github.com/xBacked-DAO/reach-safe-math/blob/main/index.rsh
const tryMul = (a, b) => {
  if (a == 0) {
    return 0
  } else {
    const c = a * b;
    if (c / a != b) {
      return 0;
    } else {
      return c;
    }
  }
};

const myFromSome = (m, def) => maybe(m, def, ((x) => x));

const MUInt = Maybe(UInt);

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    y: UInt,
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
    const mx = MUInt.Some(x);
    const my = MUInt.Some(y);
  });
  A.publish(x, y, mx, my);
  const a1 = tryMul(x, y);
  const c1 = x + y;
  const b1 = tryMul(x, y);
  const mx1 = myFromSome(mx, 0);
  const d1 = y + x;
  const my1 = myFromSome(my, 0);
  require(mx1 == x);
  require(my1 == y);
  commit();
  A.only(() => {
    const [a2, b2, c2, d2] = [a1, b1, c1, d1];
  });
  A.publish(a2, b2, c2, d2);
  require(a1 == a2);
  require(b1 == b2);
  require(c1 == c2);
  require(d1 == d2);
  commit();
  exit();
});


