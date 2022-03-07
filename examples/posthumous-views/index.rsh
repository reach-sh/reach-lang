'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    observeNumber: Fun([], Null),
  });
  const NumView = View('Number', {
    number: UInt,
  });
  init();

  Array.iota(10).forEach((n) => {
    A.publish();
    NumView.number.set(n);
    A.interact.observeNumber();
    commit();
  });

  A.publish();
  commit();
  
  exit();
});
