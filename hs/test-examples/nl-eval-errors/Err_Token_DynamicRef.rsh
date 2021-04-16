'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getToken: Fun([], Tuple(Token, UInt)),
    }),
  ],
  (A) => {
    A.only(() => {
      const [ tokenA, amtA ] = declassify(interact.getToken()); });
    A.publish(tokenA, amtA)
     .pay([ [amtA, tokenA] ]);
    commit();

    closeToks(A, [tokenA]);
  });
