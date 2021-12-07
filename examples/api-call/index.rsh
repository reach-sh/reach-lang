'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = API('Bob', {
    checkEq: Fun([UInt, UInt], Bool),
    payMe: Fun([UInt], UInt),
    noop: Fun([], Bool),
  });
  deploy();

  A.publish();
  commit();

  // Call API with destructed arguments
  const [ [ x, r ], k1 ] =
    call(B.checkEq)
      .assume((m, n) => assume(m == n));

  k1(x == r);

  commit();


  // Call API with single argument
  const [ amtTup, k2 ] =
    call(B.payMe)
      .pay((a) => a)

  k2(x);
  transfer(amtTup[0]).to(A);

  commit();

  // Call API with no domain and timeout
  try {
    const [ _, k3 ] =
      call(B.noop)
        .assume(() => assume(true))
        .throwTimeout(relativeTime(1024));

    k3(false);
  } catch (_) {
    Anybody.publish();
  }

  commit();

});
