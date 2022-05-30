'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A1 = Participant('Alice', {});
  const A2 = Participant('Bob', {});
  init();

  A1.publish();
  commit();

  A2.publish();
  commit();

  exit();
});
