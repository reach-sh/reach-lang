'reach 0.1';

const MUInt = Maybe(UInt);
export const main = Reach.App(() => {
  const common = {
    get: Fun([], UInt),
  };
  const A = Participant('Alice', common);
  const B = Participant('Bob', common);
  deploy();

  A.publish();
  const m = new Map(UInt);
  commit();

  A.only(() => { const a = declassify(interact.get()); });
  A.publish(a);
  m[A] = a;
  commit();

  B.only(() => { const b = declassify(interact.get()); });
  B.publish(b);
  m[B] = b;
  commit();

  A.only(() => { const ap = a; });
  A.publish(ap);
  // This is false because Bob might be Alice
  require(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  commit();

  exit();
});
