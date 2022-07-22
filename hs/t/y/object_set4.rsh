'reach 0.1';

export const main = Reach.App(() => {
  const O = Object({a: UInt, b: UInt, c: UInt});
  const A = Participant('A', { get: Tuple(O, UInt) });
  const E = Events({ put: [O] });
  init();

  A.only(() => { const [o, n] = declassify(interact.get); });
  A.publish(o, n);
  commit();

  A.publish();
  E.put(Object.set(o, "b", n));
  commit();
});
