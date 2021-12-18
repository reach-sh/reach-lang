'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Token,
    y: Token,
  });
  deploy();
  A.only(() => {
    const x = declassify(interact.x);
    const y = declassify(interact.y);
  });
  A.publish(x);
  commit();
  A.publish(y);
  commit();
  exit();
});
