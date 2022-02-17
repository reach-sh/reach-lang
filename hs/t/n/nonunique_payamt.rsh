'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant("A", {
    tokens: Array(Token, 5),
    condish: Bool,
  });
  init();

  A.only(() => {
    const tokens = declassify(interact.tokens);
    const token1 = tokens[0];
    const token2 = tokens[1];
    const condish = declassify(interact.condish);
    check(token1 != token2);
  });
  A.publish(token1, token2, condish);
  check(token1 != token2);
  commit();

  const tokA = condish ? token1 : token2;

  A.pay([
    [ 1, tokA ],
    [ 1, token2 ]
  ]);

  transfer(balance(token1), token1).to(A);
  transfer(balance(token2), token2).to(A);
  commit();
});
