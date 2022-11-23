'reach 0.1';

const BS = Bytes(66);
const MO = Maybe(Object({ b: Bool }));

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    b: BS,
    m: MO,
    v: Array(UInt, 3),
    chk: Fun([Array(Maybe(UInt), 3)], Null),
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    const b = declassify(interact.b);
    const m = declassify(interact.m);
    const v = declassify(interact.v);
  });
  A.publish(x, b, m, v);

  const m1 = new Map(UInt, UInt);
  const m2 = new Map(BS, UInt);
  const m3 = new Map(MO, UInt);

  m1[x] = v[0];
  m2[b] = v[1];
  m3[m] = v[2];

  commit();

  A.only(() => {
    const xp = declassify(interact.x);
    const bp = declassify(interact.b);
    const mp = declassify(interact.m);
  });
  A.publish(xp, bp, mp);

  commit();

  A.interact.chk(array(Maybe(UInt), [m1[xp], m2[bp], m3[mp]]));

  A.publish();
  delete m1[x];
  delete m2[b];
  delete m3[m];
  commit();

});
