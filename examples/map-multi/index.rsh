'reach 0.1';

const MUInt = Maybe(UInt);
const MBool = Maybe(Bool);
export const main = Reach.App(() => {
  const common = {
    get: Fun([], UInt),
    check: Fun(true, Null),
  };
  const A = Participant('Alice', common);
  const B = Participant('Bob', common);
  deploy();

  A.publish();
  const m = new Map(UInt);
  const n = new Map(Bool);
  A.interact.check(m[A], MUInt.None());
  A.interact.check(n[A], MBool.None());
  commit();

  A.only(() => {
    const a = declassify(interact.get()); });
  A.publish(a);
  m[A] = a;
  A.interact.check(m[A], MUInt.Some(a));
  A.interact.check(n[A], MBool.None());
  commit();

  A.publish();
  n[A] = isSome(m[A]);
  A.interact.check(m[A], MUInt.Some(a));
  A.interact.check(n[A], MBool.Some(true));
  commit();

  A.publish();
  delete m[A];
  A.interact.check(m[A], MUInt.None());
  A.interact.check(n[A], MBool.Some(true));
  commit();

  A.publish();
  delete n[A];
  A.interact.check(m[A], MUInt.None());
  A.interact.check(n[A], MBool.None());
  commit();

  exit();
});
