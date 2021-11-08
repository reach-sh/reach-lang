'reach 0.1';

const N = 16;

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: UInt,
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.x); });
  A.publish(x).pay(N * x);
  Array.replicate(N, null).forEach((_) => {
    transfer(x).to(A);
  });
  commit();

  exit();
});
