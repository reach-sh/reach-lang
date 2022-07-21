'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Array(Token, 2),
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  Token.track(x[0]);
  commit();
  A.publish();
  Token.track(x[1]);
  commit();
  exit();
});
