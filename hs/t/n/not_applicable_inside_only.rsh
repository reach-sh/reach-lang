'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: Bool });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const y = UInt(x);
  });
  A.publish(y);
});
