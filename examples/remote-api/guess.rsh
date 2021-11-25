'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Fun([], UInt),
  });
  const B = API({ guess: Fun([UInt], Bool) });
  deploy();

  A.only(() => {
    const x = declassify(interact.x());
  })
  A.publish(x);

  const [ guessed ] =
    parallelReduce([ false ])
      .invariant(balance() == 0)
      .while(!guessed)
      .api(B.guess,
        (msg, k) => {
          const isCorrect = msg == x;
          k(isCorrect);
          return [ isCorrect ];
        })
      .timeout(false);

  commit();

});
