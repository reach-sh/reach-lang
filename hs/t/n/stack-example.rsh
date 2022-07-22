'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  const g = (y, z) => y + z;
  const f = (x) => g(x, "five");
  const ten = f(5);
  commit();
  exit();
});
