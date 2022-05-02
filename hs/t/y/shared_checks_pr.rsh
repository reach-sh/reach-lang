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
    go2: Fun([UInt], Null),
    go3: Fun([UInt], Null)
  });
  init();

  A.only(() => {
    const token = declassify(interact.token);
  })
  A.publish(token);

  const S = new Set();

  const [ x ] = parallelReduce([ 0 ])
    .while(x < 5)
    .invariant(true)
    .paySpec([ token ])
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
      (_) => [ 0, [0, token] ],
      (msg) => {
        S.insert(this);
        require(inc == 1, "inc == 1");
        return [ msg + inc ];
      }
    )
    .api_(B.go1,
      (xp) => {
        const r = 3;
        check(xp > 0, "B.go1 checks xp > 0");
        return [ [ xp, [ 0, token ] ], (k) => {
          k(null);
          return [ r ];
        }];
      }
    )
    .api_(B.go2,
      (xp) => {
        const r = 3;
        check(xp > 0, "B.go2 checks xp > 0");
        // pay spec can access `r` binding
        return [ [ r, [ 0, token ] ], (k) => {
          k(null);
          return [ r ];
        }];
      }
    )
    .api_(B.go3,
      (xp) => {
        const r = 3;
        check(xp > 0, "B.go3 checks xp > 0");
        // gets default pay if one arg
        return [ (k) => {
          k(null);
          return [ r ];
        }];
      }
    )
    .timeout(false);

  transfer(balance()).to(A);
  transfer(balance(token), token).to(A);
  commit();


});
