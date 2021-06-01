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

  A.only(() => { const a = declassify(interact.get()); });
  A.publish(a);
  const m = new Map(UInt);
  A.interact.check(m[A], MUInt.None());
  commit();

  B.only(() => { assume(A != this); });
  B.only(() => { const b = declassify(interact.get()); });
  B.publish(b);
  require(A != B);
  m[A] = a;
  A.interact.check(m[A], MUInt.Some(a));
  A.interact.check(m[B], MUInt.None());
  commit();

  A.only(() => { const ap = a; });
  A.publish(ap);
  m[B] = b;
  A.interact.check(m[B], MUInt.Some(b));
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
