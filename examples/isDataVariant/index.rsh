'reach 0.1'

export const DT = Data({Aardvark: Null, Bird: Null});

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
  const isDV = isDataVariant("Aardvark", ["Aardvark", "Bird"], x);
  const y = isDataVariant("Bird", ["Aardvark", "Bird"], DT.Bird());
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
