'reach 0.1';
'use strict';

export const main = Reach.App(
  {},
  [ Participant('Alice', {
      getSwap: Fun([], Tuple(Token, UInt, UInt)),
      confirm: Fun([Token, UInt], Bool),
    }),
    Participant('Bob', {
      accSwap: Fun([Token, UInt], Tuple(Token, UInt)),
    }),
  ],
  (A, B) => {
    A.only(() => {
      const [ tokenA, amtA, time ] = declassify(interact.getSwap()); });
    A.publish(tokenA, amtA, time)
     .pay([ [amtA, tokenA] ]);
    commit();

    B.only(() => {
      const [tokenB, amtB] = declassify(interact.accSwap(tokenA, amtA));
      assume(tokenA != tokenB); });
    B.publish(tokenB, amtB)
     .pay([ [amtB, tokenB] ])
     .timeout(time, () => {
       A.publish();
       transfer(balance(tokenA), tokenA).to(A);
       commit();
       exit();
     });
    commit();

    A.only(() => {
      const ok = declassify(interact.confirm(tokenB, amtB)); });
    A.publish(ok)
     .timeout(time, () => {
       B.publish();
       transfer(balance(tokenA), tokenA).to(B);
       transfer(balance(tokenB), tokenB).to(B);
       commit();
       exit();
     });

    const [ recvA, recvB ] =
      ok ? [ B, A ] : [ A, B ];
    transfer(amtB, tokenB).to(recvB);
    transfer([ [amtA, tokenA] ]).to(recvA);
    commit();

    exit();
  }
);
