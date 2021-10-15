'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    cost: UInt,
  });
  const B = Participant('Bob', {
    gimmeSomeDough: Fun([Address], Null),
  });
  deploy();

  A.only(() => {
    const cost = declassify(interact.cost) });
  A.publish(cost).pay(cost);
  commit();

  B.only(() => {
    interact.gimmeSomeDough(getAddress());
  });
  B.publish();

  const x = getUntrackedFunds();

  transfer(x).to(A);
  transfer(cost).to(A);
  commit();

  exit();
});
