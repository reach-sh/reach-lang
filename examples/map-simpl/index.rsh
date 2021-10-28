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
  commit();

  A.only(() => {
    const a = declassify(interact.get());
    const ap = a;
  });
  A.publish(a, ap);
  m[A] = a;
  require(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  delete m[A];
  commit();

  exit();
});
