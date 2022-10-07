'reach 0.1';

/*
  Before Floating Opt:

  let v73 : Bool;
  const v74 : Bool* = x/69 < 4;
  local(Just v73 : Bool) if v74 then {
    let v75 : Bool;
    const v76 : Bool* = x/69 < 3;
    local(Just v75 : Bool) if v76 then {
      const v77 : Bool* = x/69 > 1;
      v75 : Bool = v77;
    } else {
      const v78 : Bool* = y/70 == 2;
      v75 : Bool = v78;
    };
    let v79 : Bool;
    const v80 : Bool* = y/70 > 2;
    local(Just v79 : Bool) if v80 then {
      v79 : Bool = false;
    } else {
      const v81 : Bool* = x/69 > 1;
      v79 : Bool = v81;
    };
    const v82 : Bool* = (b/68 ? v75 : v79);
    v73 : Bool = v82;
  } else {
    const v83 : Bool* = y/70 > 1;
    v73 : Bool = v83;
  };
  const v84 : Bool* = y/70 > 2;
  const r/85 : Bool* = (b/68 ? v73 : v84);

  After:

  const v74 : Bool* = x/69 < 4;
  const v76 : Bool* = x/69 < 3;
  const v77 : Bool* = x/69 > 1;
  const v78 : Bool* = y/70 == 2;
  const v75 : Bool* = (v76 ? v77 : v78);
  const v80 : Bool* = y/70 > 2;
  const v81 : Bool* = x/69 > 1;
  const v79 : Bool* = (v80 ? false : v81);
  const v82 : Bool* = (b/68 ? v75 : v79);
  const v83 : Bool* = y/70 > 1;
  const v73 : Bool* = (v74 ? v82 : v83);
  const v84 : Bool* = y/70 > 2;
  const r/85 : Bool* = (b/68 ? v73 : v84);

*/

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
