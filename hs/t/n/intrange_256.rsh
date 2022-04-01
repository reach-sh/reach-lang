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
  const y = UInt256(x) + UInt256(115_792_089_237_316_195_423_570_985_008_687_907_853_269_984_665_640_564_039_457_584_007_913_129_639_936);
  commit();
  A.only(() => {
    const z = y;
  });
  A.publish(z);
  check(z == y);
  commit();
  exit();
});

