'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt256,
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = 5 > x;
  commit();
  exit();
});
