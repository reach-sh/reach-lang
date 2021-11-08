'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = API('Bob', {
    checkEq: Fun([UInt, UInt], Bool),
  });
  deploy();

  A.publish();
  commit();

  // Call API with destructed arguments
  const k1 =
    call(B.checkEq)
      .assume((m, n) => assume(m == n));
  commit();
});
