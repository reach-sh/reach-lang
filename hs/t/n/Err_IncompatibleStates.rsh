'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const N = 32;
  const UInts = Array(UInt, N);
  const A = Participant('A', {
    x: UInt,
    fork: Fun([UInts], Null),
  });
  const B = Participant('B', {
    ...hasConsoleLogger,
  });
  const P = API({
    f: Fun([UInts], Null),
    g: Fun([], Null),
  });
  init();

  A.only(() => {
    const ax = declassify(interact.x);
    const xs = Array.replicate(N, ax);
  });
  A.publish(xs);
  commit();
  const same = (ys) =>
    xs.forEachWithIndex((x, i) => check(x == ys[i]));

  B.only(() => {
    const ys = xs;
  });
  B.publish(ys);
  same(ys);
  B.interact.log(xs, ys);
  commit();

  A.interact.fork(xs);
  fork()
    .api(P.f,
      (zs) => same(zs),
      (_) => 0,
      (zs, k) => {
        same(zs);
        B.interact.log(xs, ys, zs);
        k(null);
		commit()
      })
    .api(P.g,
      () => 0,
      (k) => {
        k(null);
      })
  ;
  commit();

  A.publish();
  commit();

  exit();
});
