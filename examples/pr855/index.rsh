'reach 0.1';
export const main = Reach.App(() => {
  const Creator = Participant('Creator', {
    params: Object({
      amount: UInt,
      start: UInt,
      duration: UInt,
    }),
    deployed: Fun([], Null),
  });
  const P = API({
    release: Fun([], Tuple(UInt, UInt)),
  });
  init();
  Creator.only(() => {
    const { amount, start, duration } = declassify(interact.params);
    assume(duration > 0 && amount > 0 && start >= 0);
    assume(start <= UInt.max - duration);
  });
  Creator.publish(amount, start, duration).pay(amount);
  Creator.interact.deployed();
  const [ released ] = parallelReduce([ 0 ])
    .define(() => {
      const vested = (curTime) => {
        const totalAmount = balance() + released;
        if (curTime < start) {
          return 0;
        } else if (curTime >= start + duration) {
          return totalAmount;
        } else {
          return muldiv(totalAmount, (curTime - start), duration);
        }
      }
      const releasable = (time) => vested(time) - released;
    })
    .invariant(released >= 0 && releasable(thisConsensusTime()) >= 0)
    .while(balance() > 0)
    .api(P.release, (callback) => {
      const curTime = thisConsensusTime();
      const toRelease = releasable(curTime);
      transfer(toRelease).to(Creator);
      callback([toRelease, curTime]);
      return [ released + toRelease ];
    });
  commit();
  exit();
});
