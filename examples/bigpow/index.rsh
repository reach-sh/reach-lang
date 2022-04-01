'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    id: Fun([UInt], UInt),
  });
  init();
  A.publish();
  const x = pow(2, 64, 6);
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
