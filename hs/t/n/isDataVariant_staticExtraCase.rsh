'reach 0.1';

const DT = Data({A: Null, B: Null});

export const main = Reach.App(() => {
  const A = Participant('A', {
  });
  init();
  A.publish();
  const x = DT.A();
  // Extra variant
  const isDV = isDataVariant("A", ["A", "B", "C"], x);
  commit();
  exit();
});
