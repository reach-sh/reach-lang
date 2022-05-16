'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const D = Participant('D', {
    ready: Fun([], Null),
    log: Fun(true, Null),
  });
  const P = API('P', {
    put: Fun([UInt], UInt),
    get: Fun([UInt], UInt),
    done: Fun([], Null),
  });
  init();

  D.publish();
  const M = new Map(UInt);
  D.interact.ready();

  const [done, amt] = parallelReduce([false, 0])
    .invariant(balance() == amt)
    .invariant(amt == M.sum())
    .while(! done || amt > 0)
    .api_(P.done, () => {
      check(this == D);
      return [ (k) => {
        k(null);
        D.interact.log('Done when amt == 0');
        return [ true, amt ];
      }]
    })
    .api(P.put, (x) => x, (x, k) => {
      const o = fromSome(M[this], 0);
      const n = o + x;
      k(n);
      M[this] = n;
      D.interact.log(this, 'put', x, o, n);
      return [ done, amt + x ];
    })
    .api_(P.get, (x) => {
      const o = fromSome(M[this], 0);
      check(x <= o);
      return [ (k) => {
        const n = o - x;
        k(n);
        D.interact.log(this, 'get', x, o, n);
        transfer(x).to(this);
        M[this] = n;
        return [ done, amt - x];
      }];
    });
  commit();
  exit();
});
