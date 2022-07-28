'reach 0.1';

export const main = Reach.App(() => {
  const T = Tuple(UInt, UInt, UInt);
  const A = Participant('A', {
    get: T,
    put: Fun(true, Null),
  });
  init();
  A.only(() => { const t1 = declassify(interact.get); });
  A.publish(t1);
  const t2 = Tuple.set(t1, 1, 123);
  A.interact.put(t2);
  commit();
});
