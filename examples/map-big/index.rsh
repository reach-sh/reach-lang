'reach 0.1';

const T = Bytes(72); // should be 2 ALGO pages
const MT = Maybe(T);
export const main = Reach.App(() => {
  const common = {
    get: Fun([], T),
    check: Fun(true, Null),
  };
  const A = Participant('Alice', common);
  const B = Participant('Bob', common);
  deploy();

  A.publish();
  const m = new Map(T);
  A.interact.check(m[A], MT.None());
  commit();

  A.only(() => {
    const a = declassify(interact.get()); });
  A.publish(a);
  m[A] = a;
  A.interact.check(m[A], MT.Some(a));
  commit();

  B.only(() => { assume(A != this); });
  B.publish();
  require(A != B);
  A.interact.check(m[B], MT.None());
  commit();

  B.only(() => {
    const b = declassify(interact.get()); });
  B.publish(b);
  m[B] = b;
  A.interact.check(m[B], MT.Some(b));
  commit();

  A.publish();
  delete m[A];
  A.interact.check(m[A], MT.None());
  commit();

  B.publish();
  delete m[B];
  A.interact.check(m[B], MT.None());
  commit();

  exit();
});
