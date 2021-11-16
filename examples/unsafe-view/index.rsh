'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    observe: Fun(true, Null),
  });
  const V = View({
    t: Tuple(UInt, Bool),
    u: Bool
  });
  deploy();

  A.publish();
  V.t.set([ 4, false ]);
  commit();

  A.interact.observe();
  A.publish();
  commit();

  exit();
});
