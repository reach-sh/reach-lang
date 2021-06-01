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
  if ( b < a ) {
    m[B] = b;
    A.interact.check(m[B], MUInt.Some(b));
  } else {
    A.interact.check(m[B], MUInt.None());
  }
  commit();

  A.publish();
  m[B] = a + b;
  A.interact.check(m[B], MUInt.Some(a + b));
  if ( a < b ) {
    delete m[A];
    A.interact.check(m[A], MUInt.None());
  } else {
    A.interact.check(m[A], MUInt.Some(a));
  }
  commit();

  exit();
});
