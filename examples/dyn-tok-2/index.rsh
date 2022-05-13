'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tok1: Token,
    tok2: Token,
    amt: UInt,
    stop: Fun([], Bool),
    ...hasConsoleLogger,
  });
  const B = API('B', {
    changeTok: Fun([Token], Null)
  });
  const V = View('Bals', {
    tok1: UInt,
    tok2: UInt,
    currentTok: Token,
  });

  init();

  A.only(() => {
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    const amt_ = declassify(interact.amt);
    assume(tok1 != tok2);
  });
  A.publish(tok1, tok2, amt_);

  const tokens = array(Token, [tok1, tok2]);
  require(tok1 != tok2);
  A.interact.log("Ready!");

  const [ alive, currentTok, currentWinner, amt ] =
    parallelReduce([ true, tok1, A, amt_ ])
      .define(() => {
        V.tok1.set(balance(tok1));
        V.tok2.set(balance(tok2));
        V.currentTok.set(currentTok);
      })
      .paySpec([ currentTok ])
      .invariant(balance() == 0)
      .invariant(tokens.includes(currentTok))
      .while(alive)
      .case(A,
        ( ) => ({ when: declassify(interact.stop()) }),
        (_) => { return [ false, currentTok, currentWinner, amt ]; }
      )
      .api_(B.changeTok, (newTok) => {
        check(tokens.includes(newTok), "Tokens include newTok");
        return [ [ 0, [ amt, currentTok ] ], (k) => {
          k(null);
          return [ true, newTok, this, amt ];
        }];
      })
      .timeout(false);

  transfer(balance(tok1), tok1).to(currentWinner);
  transfer(balance(tok2), tok2).to(currentWinner);
  commit();
});
