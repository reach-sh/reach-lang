'reach 0.1';

export const main = Reach.App(() => {
  const T = Tuple(UInt, UInt);
  const A = Participant('A', {
    id: Fun([T], T),
  });
  init();
  A.publish();
  const x1 = pow(2, 64, 6);
  const x2 = pow(2, 64, 7);
  const x = [x1, x2];
  commit();
  A.only(() => {
    const y = declassify(interact.id(x));
    check(x == y);
  });
  A.publish(y);
  check(x == y);
  commit();
  exit();
});
