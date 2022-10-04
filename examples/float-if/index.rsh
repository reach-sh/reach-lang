'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    b: Bool,
    x: UInt,
    y: UInt,
    z: UInt,
    chk: Fun(true, Null)
  });
  init();

  A.only(() => {
    const b = declassify(interact.b);
    const x = declassify(interact.x);
    const y = declassify(interact.y);
  });
  A.publish(b, x, y);

  // If b == true, do a bunch of hooblah and see if it is equal to 2.
  const f = () => {
    if (x < 4) {
      const h = () => {
        if (y > 2) {
          return false;
        } else {
          return x > 1;
        }
      }
      const j = () => {
        if (x < 3) {
          return x > 1;
        } else {
          return y == 2;
        }
      }
      return b ? j() : h();
    } else {
      return y > 1;
    }
  };
  const g = () => y > 2;
  const r = b ? f() : g();

  A.interact.chk(r);
  commit();

});
