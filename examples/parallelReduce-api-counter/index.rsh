'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
    max: UInt,
    launched: Fun([Contract], Null),
  });
  const B = API({
    countUp: Fun([], UInt),
  });
  init();
  A.only(() => {
    const max = declassify(interact.max);
  });
  A.publish(max);
  A.interact.launched(getContract());

  const [count] = parallelReduce([0])
    .invariant(balance() == 0, "network token balance wrong")
    .while(count < max)
    .api(B.countUp, 
      () => {},
      () => 0,
      (ret) => {
        ret(count)
        return[count + 1];
      })
  commit();
  exit();
});

