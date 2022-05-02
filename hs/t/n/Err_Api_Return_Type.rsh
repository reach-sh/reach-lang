'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    when: Bool,
    token: Token
  });
  const B = API('B', {
    go1: Fun([UInt], Null),
  });
  init();

  A.publish();

  const [ x ] = parallelReduce([ 0 ])
    .while(x < 5)
    .invariant(true)
    .case(A,
      () => ({
        when: declassify(interact.when),
        msg : declassify(interact.x),
      }),
      (_) => 0,
      (msg) => { return [ msg ]; }
    )
    .api_(B.go1,
      (xp) => {
        const r = 3;
        check(xp > 0, "B.go1 checks xp > 0");
        return [ 3, xp, (k) => {
          k(null);
          return [ r ];
        }];
      }
    )
    .timeout(false);

  transfer(balance()).to(A);
  commit();


});
