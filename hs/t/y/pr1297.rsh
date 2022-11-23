'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const P = API({ f: Fun([UInt], Null) });
  init();
  A.publish();
  const bene = this;
  const m = new Map(UInt);
  const get = (w) => fromSome(m[w], 0);
  m[bene] = 10;
  const [ done ] = parallelReduce([ false ])
    .invariant(get(bene) == 10, "bene is 10")
    .invariant(balance() == 0, "balance")
    .while ( !done )
    .api_(P.f, (x) => {
      return [0, (k) => {
        m[this] = x;
        k(null);
        return [ true ];
      }]});
  commit();
  exit();
});
