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

  A.only(() => {
    const a = declassify(interact.get()); });
  A.publish(a);
  const m = new Map(UInt);
  assert(m[A] == MUInt.None());

  const read = () => maybe(m[A], 0, (x) => x);

  m[A] = a;
  require(read() == a);

  m[A] = a + 1;
  require(read() == a + 1);
  
  m[A] = a + 2;
  require(read() == a + 2);

  commit();
  A.interact.check(m[A], MUInt.Some(a+2));

  exit();
});
