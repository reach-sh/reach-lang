'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
    when: Bool
  });
  const B = API('B', {
    go: Fun([UInt], Null)
  });
  init();

  A.publish();

  const S = new Set();

  const [ x ] = parallelReduce([ 0 ])
    .while(x < 5)
    .invariant(balance() == 0)
    .case(A,
      () => {
        check(!S.member(this), "Not a member");
        const inc = 1;
      },
      () => {
        const y = declassify(interact.x);
        assume(inc == 1, "inc == 1");
        return ({
          when: declassify(interact.when),
          msg : y,
        });
      },
      (_) => 0,
      (msg) => {
        S.insert(this);
        require(inc == 1, "inc == 1");
        return [ msg + inc ];
      }
    )
    .api(B.go,
      (xp) => {
        const r = 3;
        check(xp > 0, "B.go checks xp > 0");
      },
      (_) => {
        assume(r == 3);
      },
      (_) => 0,
      (xp, k) => {
        k(null);
        return [ r ];
      }
    )
    .timeout(false);

  commit();

});
