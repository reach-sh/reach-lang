'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tok1: Token,
    tok2: Token
  });
  init();

  A.only(() => {
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    assume(tok1 != tok2);
  })
  A.publish(tok1, tok2);
  require(tok1 != tok2);

  const tokens = [tok1, tok2];

  const alive =
    parallelReduce([ true ])
    .while(true)
    .invariant(balance() == 0)
    .paySpec(tokens)
    .case(A,
      () => ({ when: true }),
      () => {
        return [ true ];
      })
    .timeout(false);

  commit();
});
