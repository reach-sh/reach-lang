'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    token: Token,
    ...hasConsoleLogger
  });
  const B = Participant('Bob', {
    gimmeSomeDough: Fun([Address], Null),
  });
  init();

  A.only(() => { const token = declassify(interact.token); })
  A.publish(token);
  const x1 = getUntrackedFunds(token);

  transfer(x1, token).to(A);
  commit();

  B.only(() => {
    interact.gimmeSomeDough(getAddress());
  });
  B.publish();

  const x = getUntrackedFunds(token);

  transfer(x, token).to(B);
  commit();

  exit();
});
