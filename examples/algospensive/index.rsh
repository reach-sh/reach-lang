'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
  });
  const B = Participant('B', {
    ...hasConsoleLogger,
  });
  const N = 32;
  init();

  A.only(() => {
    const ax = declassify(interact.x);
    const xs = Array.replicate(N, ax);
  });
  A.publish(xs);
  commit();

  B.only(() => {
    const ys = xs;
  });
  B.publish(ys);
  xs.forEachWithIndex((x, i) => check(x == ys[i]));
  B.interact.log(xs, ys);
  commit();

  exit();
});
