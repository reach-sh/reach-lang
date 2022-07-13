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

  A.pay([ [1, tok] ]);
  commit();

  A.publish();
  transfer(1, tok).to(A);
  commit();

  exit();
});
