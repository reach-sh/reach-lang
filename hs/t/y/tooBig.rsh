'reach 0.1';

export const main = Reach.App(() => {
  const D = Participant('D', { x: UInt });
  init();
  D.only(() => { const x = declassify(interact.x); });
  const N = 40;
  Array.iota(N).forEach((i) => {
    D.publish(x).pay(x);
    commit();
    D.publish();
    transfer(x).to(D);
    commit();
  });
  exit();
});
