'reach 0.1';

const f = (x) => check(x < 5, "check example");

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: UInt,
  });
  init();

  A.only(() => {
    const x = declassify(interact.x);
    f(x);
  });
  A.publish(x);
  f(x);
  commit();
  exit();
});
