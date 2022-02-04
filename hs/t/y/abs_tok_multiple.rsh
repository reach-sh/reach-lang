'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    params: Tuple(Token, Token, Token),
  });
  init();
  A.only(() => {
    const [ token1, token2, _ ] = declassify(interact.params);
    assume(token1 != token2);
  });
  A.publish(token1, token2);
  require(token1 != token2);
  commit();

  // We can verify that tokens published later are distinctive
  A.only(() => {
    const [ _, _, token3 ] = declassify(interact.params);
    assume(token3 != token1 && token3 != token2);
  });
  A.publish(token3);
  require(token3 != token1 && token3 != token2);
  commit();
});
