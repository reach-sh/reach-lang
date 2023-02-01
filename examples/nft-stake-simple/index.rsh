'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    params: Object({
      tok: Token,
      rewards: UInt,
      deadline: UInt,
    }),
    launched: Fun([Contract], Null),
    checkStatus: Fun([], Bool),
  });
  const B = Participant('User', {});
  const V = View({
    seeTerms: Tuple(Token, UInt, UInt),
  });
  init();

  A.only(() => {
    const {tok, rewards, deadline} = declassify(interact.params);
  });
  A.publish(tok, rewards, deadline)
    .pay(rewards);
  V.seeTerms.set([tok, rewards, deadline]);
  commit();
  A.interact.launched(getContract());

  B.pay([[1, tok]]);
  commit();

  wait(relativeTime(deadline));

  A.only(() => {
    const b = declassify(interact.checkStatus());
  });
  A.publish(b);

  transfer(rewards).to(B);
  transfer(1, tok).to(B);

  commit();
  exit();
});
