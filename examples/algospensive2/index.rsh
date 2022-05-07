'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const N = 64;
  const A = Participant('A', {
    x: UInt,
  });
  init();

  A.only(() => {
    const ax = declassify(interact.x);
    const xs = Array.replicate(N, ax);
  });
  A.publish(ax, xs);
  xs.forEach((x) => check(x == ax));
  commit();
  A.publish();
  commit();
  exit();
});
