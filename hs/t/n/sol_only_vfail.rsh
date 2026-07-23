'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: UInt,
  });

  init();

  A.only(() => {
    const x = declassify(interact.x);
  });
  A.publish(x);
  assert(x < 10, 'x is small');
  commit();

  exit();
});
