'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = Participant('Bob', {
    gimmeSomeDough: Fun([Address], Null),
  });
  deploy();

  A.publish();
  commit();

  B.only(() => {
    interact.gimmeSomeDough(getAddress());
  });
  B.publish();

  const x = getUntrackedFunds();

  transfer(x).to(A);
  commit();

  exit();
});
