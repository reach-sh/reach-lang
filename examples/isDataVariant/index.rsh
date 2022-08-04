'reach 0.1'

export const DT = Data({A: Null, B: Null});

export const main = Reach.App(() => {
  const A = Participant('A', {
    getDT: Fun([], DT),
    checkBool: Fun([Bool], Null),
  });
  init();

  A.only(() => {
    const x = declassify(interact.getDT());
  });
  A.publish(x);
  const isDV = isDataVariant("A", ["A", "B"], x);
  const y = isDataVariant("B", ["A", "B"], DT.B());
  commit();
  A.only(() => {
    const isDV_ = isDV;
    const z = y;
  })
  A.publish(isDV_, z);
  check(isDV_ == isDV);
  check(y == z);
  commit();
});
