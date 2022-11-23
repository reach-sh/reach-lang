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
    del: Fun([], UInt),
    done: Fun([], Null),
  });
  init();

  D.publish();
  const M = new Map(UInt);
  D.interact.ready();

  const [done, amt, cnt] = parallelReduce([false, 0, 0])
    .invariant(balance() == amt, "inv balance")
    .invariant(amt == M.sum(), "inv sum")
    .invariant(cnt == M.size(), "inv size")
    .invariant(implies(done, amt == 0), "inv done cnt")
    .invariant(implies(done, cnt == 0), "inv done amt")
    .while(! done)
    .api_(P.done, () => {
      check(this == D);
      check(amt == 0);
      check(cnt == 0);
      return [ (k) => {
        k(null);
        D.interact.log('Done when amt == 0');
        return [ true, 0, 0 ];
      }]
    })
    .api_(P.del, () => {
      const o = fromSome(M[this], 0);
      const dc = isSome(M[this]) ? 1 : 0;
      return [ (k) => {
        k(o);
        transfer(o).to(this);
        delete M[this];
        return [ false, amt - o, cnt - dc ];
      } ];
    })
    .api_(P.put, (x) => {
      const o = fromSome(M[this], 0);
      const dc = isSome(M[this]) ? 0 : 1;
      const n = o + x;
      return [ x, (k) => {
        k(n);
        M[this] = n;
        D.interact.log(this, 'put', x, o, n);
        return [ false, amt + x, cnt + dc ];
      } ];
    })
    .api_(P.get, (x) => {
      const o = fromSome(M[this], 0);
      const dc = isSome(M[this]) ? 0 : 1;
      check(x <= o);
      return [ (k) => {
        const n = o - x;
        k(n);
        D.interact.log(this, 'get', x, o, n);
        transfer(x).to(this);
        M[this] = n;
        return [ false, amt - x, cnt + dc ];
      }];
    });
  commit();
  assert(cnt == 0);
  exit();
});
