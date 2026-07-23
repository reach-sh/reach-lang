'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    amt: UInt,
  });

  init();

  A.only(() => {
    const amt = declassify(interact.amt);
  });
  A.publish(amt).pay(amt);
  transfer(amt).to(A);
  commit();

  exit();
});
