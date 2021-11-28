'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    launched: Fun([], Null),
    getAmt: Fun([], UInt),
    ready: Fun([], Null),
    inform: Fun([Bool, UInt, UInt], Null),
    done: Fun([], Null),
  });
  const U = API('User', {
    pay: Fun([], Null),
    read: Fun([], UInt),
    write: Fun([UInt], UInt),
    writeC: Fun([UInt], UInt),
    stop: Fun([], UInt),
  });
  deploy();
  A.publish();
  A.interact.launched();
  commit();
  A.only(() => {
    const amt = declassify(interact.getAmt());
  });
  A.publish(amt);
  A.interact.ready();

  const [ done, x, an ] =
    parallelReduce([ false, 0, 0 ])
    .invariant(balance() == an)
    .while( ! done )
    .api(U.pay, () => amt, (k) => {
      k(null);
      return [ done, x, an + amt ];
    })
    .api(U.read, (k) => {
      k(x);
      return [ done, x, an ];
    })
    .api(U.write, (nx, k) => {
      k(x);
      return [ done, nx, an ];
    })
    .api(U.writeC, (nx) => {
      assume(nx > x);
    }, (_) => 0, (nx, k) => {
      k(x);
      return [ done, nx, an ];
    })
    .api(U.stop, (k) => {
      k(x);
      return [ true, x, an ];
    });

  transfer(an).to(A);
  commit();
  A.interact.done();

  exit();
});
