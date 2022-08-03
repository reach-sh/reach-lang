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
  // Missing variant
  const isDV = isDataVariant("A", ["A"], x);
  commit();
  exit();
});
