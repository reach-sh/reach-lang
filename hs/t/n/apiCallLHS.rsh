'reach 0.1';

export const main = Reach.App(() => {
  const D = Participant('D', {
    x: UInt,
  });
  const A = API({
    f: Fun([UInt], Bool),
  });
  deploy();

  D.only(() => { const x = declassify(interact.x); });
  D.publish(x);
  commit();
  const [[y], k] = call(A.f)
    .assume((i) => { assume(i == y); });
  k(true);
  commit();
  exit();
});
