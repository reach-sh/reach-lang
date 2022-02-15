'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tok1: Token,
    tok2: Token,
    cond: Bool,
    amt: UInt,
    checkBal: Fun([UInt], Null)
  });

  init();

  A.only(() => {
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    const cond = declassify(interact.cond);
    const amt = declassify(interact.amt);
    assume(tok1 != tok2);
  });
  A.publish(tok1, tok2, cond, amt);

  A.interact.checkBal(0);
  require(tok1 != tok2);
  const tok = cond ? tok1 : tok2;
  commit();

  A.publish().pay([ [ amt, tok ] ]);
  A.interact.checkBal(1);
  commit();

  A.publish();

  transfer(balance(tok1), tok1).to(A);
  transfer(balance(tok2), tok2).to(A);

  A.interact.checkBal(2);
  commit();
});
