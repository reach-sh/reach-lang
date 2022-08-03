'reach 0.1';

const DT = Data({A: Null, B: Null});

export const main = Reach.App(() => {
  const A = Participant('A', {
    getDT: Fun([], DT),
  });
  init();
  A.only(() => {
    const x = declassify(interact.getDT());
  });
  A.publish(x);
  // Extra variant
  const isDV = isDataVariant("A", ["A", "B", "C"], x);
  commit();
  exit();
});
