'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    x: Bytes(4),
  });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    const o = Object.has({}, x);
  });
  A.publish(o);
  exit();
});
