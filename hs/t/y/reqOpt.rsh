'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    check(x > 5);
  });
  A.publish(x);
  check(x > 5);
  if ( x > 5 ) {
    commit();
    A.publish();
  }
  commit();
  exit();
});

