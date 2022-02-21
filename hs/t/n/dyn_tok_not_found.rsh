'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tok1: Token,
    tok2: Token,
    stop: Fun([], Bool),
    ...hasConsoleLogger,
  });
  const B = API('B', {
    changeTok: Fun([Token], Null)
  });

  init();

  A.only(() => {
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    assume(tok1 != tok2);
  });
  A.publish(tok1, tok2);

  const tokens = array(Token, [tok1, tok2]);
  require(tok1 != tok2);

  // Allow payments in parallelReduce to be of any form (no `paySpec`)
  const [ alive, currentWinner ] =
    parallelReduce([ true, A ])
      .invariant(balance() == 0)
      .while(alive)
      .case(A,
        ( ) => ({ when: declassify(interact.stop()) }),
        (_) => [ 0, [ 1, tok1 ] ],
        (_) => { return [ false, currentWinner ]; }
      )
      .api(B.changeTok,
        // We do not assume/require that `payTok` is tracked
        (payTok) => {  },
        (payTok) => [ 0, [ 1, payTok ] ],
        (payTok, k) => {
          k(null);
          return [ true, this ];
        }
      )
      .timeout(false);

  transfer(balance(tok1), tok1).to(currentWinner);
  transfer(balance(tok2), tok2).to(currentWinner);
  commit();
});
