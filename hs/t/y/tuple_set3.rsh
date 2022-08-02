'reach 0.1';

export const main = Reach.App(() => {
  const T1 = Tuple(UInt, UInt, UInt);
  const A = Participant('A', { get: Tuple(T1, UInt) });
  const E = Events({ put: [T1] });
  init();

  A.only(() => { const [t1, n] = declassify(interact.get); });
  A.publish(t1, n);
  commit();

  A.publish()
  const t2 = Tuple.set(t1, 1, n);
  E.put(t2);
  commit();});
