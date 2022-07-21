'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
  });
  const B = API({
    t: Fun([Token], Null)
  })
  init();
  A.publish();
  commit();

  const [ [t1], k1 ] = call(B.t);
  k1(null);
  Token.track(t1);
  commit();

  const [ [t2], k2 ] = call(B.t);
  k2(null);
  Token.track(t2);
  commit();
});
