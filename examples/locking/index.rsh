'reach 0.1';
export const server = Reach.App(() => {
  const A = Participant('Admin', {
    ready: Fun([Contract], Null),
  });
  const V = View({ x: UInt });
  const P = API({
    f: Fun([], UInt),
    g: Fun([Contract, Bool], UInt),
    o: Fun([], Null),
  });
  init();
  A.publish();
  A.interact.ready(getContract());
  const [ x, done ] =
    parallelReduce([ 42, false ])
    .invariant(balance() == 0)
    .while( !done )
    .define(() => {
      V.x.set(x);
    })
    .api_(P.o, () => {
      return [ 0, (k) => {
        k(null);
        return [ x, true ];
      }];
    })
    .api_(P.f, () => {
      return [ 0, (k) => {
        k(x);
        return [ x + 1, done ];
      }];
    })
    .api_(P.g, (c, b) => {
      return [ 0, (k) => {
        const r = remote(c, {
          f: Fun([Bool], UInt),
        });
        k(r.f(b));
        return [ x, done ];
      }];
    });
  commit();
  exit();
});
