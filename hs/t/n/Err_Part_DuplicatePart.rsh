'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A1 = Participant('Alice', {});
  const A2 = Participant('Alice', {});
  deploy();
  exit();
});
