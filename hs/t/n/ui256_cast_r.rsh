'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt256,
    z: UInt,
  });
  init();
  A.only(() => {
    const z = declassify(interact.z);
    const x = declassify(interact.x);
  });
  A.publish(z, x);
  const y = UInt(z) + UInt(x);
  commit();
  exit();
});
