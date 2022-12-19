'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  const m = new Map(UInt, UInt);

  var x = 0;
  invariant(balance() == 0);
  invariant(m.reduceWithKey(true, (z, k, v) =>
    z && k == v));
  invariant(m.size() == x);
  while (x < 4) {
    commit();
    A.publish();
    enforce(isNone(m[x]));
    m[x] = x;
    x = x + 1;
    continue;
  }

  var y = x;
  invariant(balance() == 0);
  invariant(m.reduceWithKey(true, (z, k, v) =>
    z && k == v));
  invariant(m.size() == y);
  while (y != 0) {
    commit();
    A.publish();
    enforce(isSome(m[y - 1]));
    delete m[y - 1];
    y = y - 1;
    continue;
  }
  commit();
  exit();
});
