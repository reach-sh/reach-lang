'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const C = Participant('C', {});
  const A = API('A', {
    f: Fun([UInt], UInt),
  });
  deploy();
  C.publish();
  var [] = [];
  invariant(balance() == 0);
  while (true) {
    commit();
    fork().api(A.f, (x, k) => {
      k(x);
      continue;
    });
  };
  commit();
  exit();
});
