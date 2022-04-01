'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = x + 18_446_744_073_709_551_616;
  commit();
  A.only(() => {
    const z = y;
  });
  A.publish(z);
  check(z == y);
  commit();
  exit();
});
