'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: Bool,
  });
  deploy();

  A.only(() => {
    const x = declassify(interact.x); });
  A.publish(x); commit();

  wait(x ? relativeTime(10) : relativeSecs(10));

  A.publish(); commit();
  exit();
});
