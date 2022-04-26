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

  const [ x ] = parallelReduce([ 0 ])
    .while(x < 5)
    .invariant(balance() == 0)
    .case(A,
      // publish
      () => {
        const y = declassify(interact.x);
        return ({
          when: declassify(interact.when),
          msg : y,
        });
      },
      // check block
      () => {
        check(y > 0, "A checks x > 0");
      },
      // pay (optional)
      // consensus
      (msg) => {
        return [ msg + 1 ];
      }
    )
    .api(B.go,
      // assume
      () => {}
      // pay (optional)
      // consensus
    )
    .timeout(false);

  commit();

});
