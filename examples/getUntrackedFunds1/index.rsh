'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = Participant('Bob', {
    gimmeSomeDough: Fun([Address], Null),
  });
  init();

  A.publish();
  const x1 = getUntrackedFunds();

  transfer(x1).to(A);
  commit();

  B.only(() => {
    interact.gimmeSomeDough(getAddress());
  });
  B.publish();

  const x = getUntrackedFunds();

  transfer(x).to(B);
  commit();

  exit();
});
