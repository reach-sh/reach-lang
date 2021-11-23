'reach 0.1';

export const main = Reach.App(() => {
  const CommonInterface = {
    isObserving: Fun([UInt], Bool),
    log: Fun(true, Null)
  };

  const A = Participant('Alice', {
    ...CommonInterface,
    doAliceStuff: Fun([], Null)
  });
  const B = Participant('Bob', {
    ...CommonInterface,
    doBobStuff: Fun([], Null)
  });
  const LCT = View('LCT', {
    current: UInt
  });
  deploy();

  A.publish();
  commit();
  B.publish();

  const [i] = parallelReduce([0])
    .define(() => {
      LCT.current.set(lastConsensusTime());
    })
    .invariant(balance() == 0)
    .while(i < 3)
    .case(A,
      () => {
        interact.log("Alice case", i);
        return {
          when: declassify(!interact.isObserving(lastConsensusTime())),
          msg: declassify(interact.doAliceStuff())
        }
      },
      (_) => 0,
      (_) => {
        return [i + 1];
      })
    .case(B,
      () => {
        interact.log("Bob case", i);
        return {
          when: declassify(!interact.isObserving(lastConsensusTime())),
          msg: declassify(interact.doBobStuff())
        }
      },
      (_) => 0,
      (_) => {
        return [i + 1];
      })
    .timeout(relativeSecs(10), () => {
      A.interact.log("Alice time b", i);
      B.interact.log("Bob time b", i);
      Anybody.publish();
      A.interact.log("Alice time a", i);
      B.interact.log("Bob time a", i);
      return [i+1];
    });

  commit();
  A.interact.log("Alice out!");
  B.interact.log("Bob out!");
  exit();
});
