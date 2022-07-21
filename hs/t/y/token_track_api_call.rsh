'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API({ getToken: Fun([Token], Null) });
  init();

  A.publish();
  commit();

  const [ [tok], k ] = call(B.getToken);
  k(null);
  tok.track();
  commit();

  const [ [tok2], k2 ] = call(B.getToken).check((tok2) => { check(distinct(tok, tok2)); });
  k2(null);
  tok2.track();
  commit();

  A.pay([ [1, tok] ]);
  commit();

  A.publish();
  transfer(1, tok).to(A);
  commit();

  exit();
});
