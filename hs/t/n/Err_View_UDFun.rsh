'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const V = View('Test', {
    log: Fun(true, Null),
  });
  deploy();

  exit();
});

