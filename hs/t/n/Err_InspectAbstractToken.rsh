'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    params: Tuple(Token),
  });
  init();
  A.only(() => {
    const [ token ] = declassify(interact.params);
    const mt = Maybe(Token).Some(token);
  });
  A.publish(mt);
  commit();

  A.pay([ maybe(mt, 0, (tok) => [ 5, tok ]) ]);

  commit();
});
