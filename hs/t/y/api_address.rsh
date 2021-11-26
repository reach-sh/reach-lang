'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Fun([], UInt),
  });
  const B = API({ pay: Fun([Address], Null) });
  deploy();

  A.only(() => {
    const x = declassify(interact.x());
  })
  A.publish(x);

  const [ paid ] =
    parallelReduce([ false ])
      .invariant(balance() == 0)
      .while(!paid)
      .api(B.pay,
        _ => 2,
        (a, k) => {
          transfer(2).to(a);
          k(null);
          return [ true ];
        })
      .timeout(false);

  commit();

});
