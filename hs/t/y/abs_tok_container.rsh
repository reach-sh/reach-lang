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

  A.pay([[ 5, token1 ], [ 5, token2 ]]);

  // Can put tokens in container and use them later
  const x = array(Token, [token1, token2]);
  commit();

  A.publish();
  transfer(5, x[0]).to(A);
  transfer(5, x[1]).to(A);
  commit();
});
