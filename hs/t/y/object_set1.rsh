'reach 0.1';

export const main = Reach.App(() => {
  const O = Object({a: UInt, b: UInt, c: UInt});
  const A = Participant('A', {
    get: O,
    put: Fun(true, Null),
  });
  init();
  A.only(() => { const o1 = declassify(interact.get); });
  A.publish(o1);
  const o2 = Object.set(o1, "b", 123);
  A.interact.put(o2);
  commit();
});
