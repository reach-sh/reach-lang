'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: UInt,
    f: Fun([], Null),
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.x);
    if ( x > 5 ) { interact.f(); }
    const g = () => {
      if ( x > 15 ) { return 10; }
      return (x > 20) ? 0 : 10;
    };
    const y = g();
    assert(y == 10);
    const n = x + y;
  });
  A.publish(n);
  commit();
  exit();
});
