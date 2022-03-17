'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const O = Participant('Observer', {});
  void O;
  const NumEv = Events('Numbers', {
    number: [UInt],
  });
  init();

  Array.iota(10).forEach((n) => {
    A.publish();
    NumEv.number(n);
    commit();
  });

  exit();
});
