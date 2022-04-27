'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    x;
  });
  A.publish(x);
  commit();
})
