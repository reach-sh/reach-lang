'reach 0.1';

const MUInt = Maybe(UInt);
export const main = Reach.App(() => {
  const common = {
    get: Fun([UInt], UInt),
  };
  const A = Participant('A', common);
  init();

  A.publish();
  const m = new Map(UInt);
  commit();

  A.only(() => {
    const a = declassify(interact.get(1));
    const ap = a;
  });
  A.publish(a, ap);
  m[A] = a;
  require(m[A] == MUInt.Some(ap), "m[A] is Some(ap)");
  commit();

  A.only(() => {
    const b = declassify(interact.get(2));
  });
  A.publish(b);
  require(m[A] == MUInt.Some(ap));
  delete m[A];
  commit();

  exit();
});
