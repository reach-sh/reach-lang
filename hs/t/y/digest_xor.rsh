'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    d: Digest,
  });
  init();
  A.only(() => {
    const d1 = declassify(interact.d);
    const d2 = digest("food");
    const d3 = d1 ^ d2;
  });
  A.publish(d1, d2, d3);
  const d4 = d1 ^ d2;
  check(d3 == d4);
  commit();
});
