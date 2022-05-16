'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    checkDecimals: Fun(true, Null),
    decimals: UInt,
  });
  const TLE = Events({ tokenLaunch: [] });
  init();

  A.only(() => {
    const decimals = declassify(interact.decimals);
  });
  A.publish(decimals);

  const supply = UInt.max;
  const t = new Token({ supply, decimals });
  TLE.tokenLaunch();

  commit();
  A.interact.checkDecimals(t);
  A.publish();

  t.burn(supply);
  t.destroy();

  commit();
});
