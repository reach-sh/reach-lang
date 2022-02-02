'reach 0.1';

const MT = Maybe(Token);
const MS = MT.Some

const mkPayAmt = (tok) => [ balance(tok), tok ];

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    params  : Tuple(Token, Token, UInt),
    checkBal: Fun(true, Null)
  });
  init();

  A.only(() => {
    const [ token1, token2, amt ] = declassify(interact.params);
    assume(token1 != token2);
  });
  A.publish(token1, token2, amt);
  require(token1 != token2);
  commit();

  const chkBal = (i) => {
    A.interact.checkBal(i, balance(token1), balance(token2));
  };

  A.publish()
   .pay([ [ amt, token1 ], [ amt, token2 ] ]);

  chkBal(0);

  const go = (mtok) => {
    const payAmt = fromMaybe(mtok, balance, mkPayAmt);
    transfer(...payAmt).to(A);
  }

  go(MS(token1));

  chkBal(1);

  transfer(balance(token2), token2).to(A);

  commit();
});
