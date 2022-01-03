'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    tok1: Token,
    tok2: Token,
    tok3: Token,
  });
  init();

  A.only(() => {
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    const tok3 = declassify(interact.tok3);
    assume(tok1 != tok2); assume(tok2 != tok3); assume(tok1 != tok3);
  });
  A.publish(tok1, tok2, tok3);
  commit();

  const pa = [1, [1, tok1], [1, tok2], [1, tok3]];
  A.pay(pa);
  transfer(pa).to(A);
  commit();

  exit();
});
