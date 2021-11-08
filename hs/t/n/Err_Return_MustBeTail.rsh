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
      if ( x > 20 ) {
        return 0;
      } else {
        return 10;
      }
      return 12;
    };
    const n = x + g();
  });
  A.publish(n);
  commit();
  exit();
});
