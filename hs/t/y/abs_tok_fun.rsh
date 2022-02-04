'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    params: Tuple(Token, Token),
  });
  init();
  A.only(() => {
    const [ token1, token2 ] = declassify(interact.params);
    assume(token1 != token2);
  });
  A.publish(token1, token2);
  require(token1 != token2);
  commit();

  const choose = (x) => {
    if (x) {
      return token1;
    } else {
      return token2;
    }
  }

  A.pay([ [ 25, token2 ] ]);

  // Can return token from function when result is known at compile time
  const which = choose(false);
  commit();

  A.publish();
  transfer(balance(which), which).to(A);
  commit();
});
