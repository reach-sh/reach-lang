'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tokens: Tuple(Token, Token),
  });
  deploy();

  A.only(() => {
    const [tokA, tokB] = declassify(interact.tokens);
    assume(tokA != tokB);
  });
  A.publish(tokA, tokB);
  require(tokA != tokB);

  const [i] =
    parallelReduce([ 0 ])
      .invariant(balance() == 0 && balance(tokA) == 0 && balance(tokB) == 0)
      .while(i < 10)
      .paySpec([tokA, tokB])
      .case(A,
        (() => ({ when: true })),
        (() => { return [ i + 1 ]; })
      )
      .timeout(1024, () => {
        A.publish();
        return [ i ];
      });

  commit();

});
