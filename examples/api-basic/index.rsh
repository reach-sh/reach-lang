'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    ...hasConsoleLogger,
  });
  deploy();

  A.publish();
  commit();

  exit();
});
