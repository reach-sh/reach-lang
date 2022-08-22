'reach 0.1';

export const main = Reach.App(() => {
  const B = Bytes(1);
  const A = Participant('A', { dc: Fun([], Null) });
  const V = View({ identityViewFun: Fun([B], B) });
  init();
  A.publish();
  V.identityViewFun.set((x) => x);
  commit();

  A.interact.dc();
  A.publish();
  commit();
});
