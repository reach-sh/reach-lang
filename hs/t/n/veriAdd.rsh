'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt, ...hasConsoleLogger });
  init();
  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  const y = veriAdd(x, 5);
  A.interact.log(y);
  commit();
});
