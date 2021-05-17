'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const tA = [ Participant('A', {}) ];
  deploy();

  tA[0].publish();
  commit();
  exit();
});
