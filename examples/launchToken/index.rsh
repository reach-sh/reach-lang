'reach 0.1';
'use strict';
export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  commit();
  exit();
});
