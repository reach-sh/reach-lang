'reach 0.1';

const MUInt = Maybe(UInt);
export const main = Reach.App(() => {
  const common = {
    get: Fun([], UInt),
    check: Fun([MUInt, MUInt], Null),
  };
  const A = Participant('Alice', common);
  const B = Participant('Bob', common);
  deploy();

  A.publish();
  const m = new Map(UInt);
  A.interact.check(m[A], MUInt.None());
  commit();

  A.only(() => { const a = declassify(interact.get()); });
  A.publish(a);
  m[A] = a;
  A.interact.check(m[A], MUInt.Some(a));
  commit();

  B.only(() => { assume(A != this); });
  B.publish();
  require(A != B);
  A.interact.check(m[B], MUInt.None());
  commit();

  B.only(() => { const b = declassify(interact.get()); });
  B.publish(b);
  m[B] = b;
  A.interact.check(m[B], MUInt.Some(b));
  commit();

  A.only(() => { const ap = a; });
  A.publish(ap);
  require(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  delete m[A];
  A.interact.check(m[A], MUInt.None());
  commit();

  B.only(() => { const bp = b; });
  B.publish(bp);
  require(m[B] == MUInt.Some(bp));
  delete m[B];
  A.interact.check(m[B], MUInt.None());
  commit();

  exit();
});
