'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    mx: Maybe(UInt),
    ma: Array(Digest, 10),
  });
  init();

  A.only(() => {
    const mx = declassify(interact.mx);
    const ma = declassify(interact.ma);
  });
  A.publish(mx, ma);

  const any = ma.any(m => m == digest(mx));
  const all = ma.all(m => m == digest(mx));

  commit();

});
