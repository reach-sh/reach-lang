'reach 0.1'

export const DT = Data({A: Null, B: Null});

export const main = Reach.App(() => {
  const A = Participant('A', {
    getDT: Fun([], DT),
  });
  init();

  A.only(() => {
    const x = declassify(interact.getDT());
  });
  A.publish(x);
  const isDV = isDataVariant("A", ["A", "B"], x);
  enforce(isDV);
  const y = isDataVariant("B", ["A", "B"], DT.B());
  commit();
  A.only(() => {
    const z = y;
  })
  A.publish(z);
  check(y == z);
  commit();
});
