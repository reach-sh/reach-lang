'reach 0.1';
'use strict';

function closeToks(Who, toks, after = (() => null)) {
  Who.publish();
  transfer(balance()).to(Who);
  const atoks = array(Token, toks);
  Foldable_forEach(atoks, (tok) => transfer(balance(tok), tok).to(Who));
  commit();
  after();
  exit();
};

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
