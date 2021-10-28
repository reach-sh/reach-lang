'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    observe: Fun([], Null),
  });
  const V = View("V", {
    iV: UInt,
  });
  deploy();

  A.publish();

  var [ i ] = [ 0 ];
  invariant(balance() == 0);
  while (i < 5) {
    commit();
    A.interact.observe();
    A.publish();
    V.iV.set(i);
    i = i + 1;
    continue;
  }
  commit();
});
