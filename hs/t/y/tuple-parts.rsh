'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const tA = [ Participant('A', {}) ];
  init();

  tA[0].publish();
  commit();
  exit();
});
