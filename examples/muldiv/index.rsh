'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    x: UInt,
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.x);
    const z = muldiv(x, 4, 5);
  });
  A.publish(x, z);

  const y = muldiv(x, 4, 5);

  require(y == z);

  commit();

  const a = muldiv(x, 4, 5);
  A.interact.log("a", a);

});
