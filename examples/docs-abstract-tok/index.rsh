'reach 0.1';

const MT = Maybe(Array(Token, 2));
const MS = MT.Some

const mkPayAmt = (tok) => [ balance(tok), tok ];

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    params  : Tuple(Token, Token, UInt),
    token3  : Token,
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

  const getTok = (a) => (a > 0) ? token1 : token2;

  const go = (mtok) => {
    mtok.match({
      Some: ([ t1, t2 ]) => {
        transfer(balance(t1), t1).to(A);
      },
      None: () => {}
    });
  }

  const ta = array(Token, [ getTok(4), token2 ]);
  go(MS(ta));

  chkBal(1);

  transfer(balance(token2), token2).to(A);

  commit();

  A.only(() => {
    const token3 = declassify(interact.token3);
    assume(token3 != token1 && token3 != token2);
  });
  A.publish(token3);
  require(token3 != token1 && token3 != token2);
  commit();

});
