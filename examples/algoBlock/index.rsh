'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    ...hasConsoleLogger,
    delay: Fun([UInt], Null),
  });
  init();

  A.publish();
  commit();

  const f = (shouldBeSome) => {
    const g = (l, h) => {
      const x = h(lastConsensusTime());
      A.interact.log(l, x);
      enforce(isSome(x) == shouldBeSome, l);
    };
    g('Seed', ALGO.blockSeed);
    g('Secs', ALGO.blockSecs);
  };

  A.publish();
  f(true);
  commit();

  A.publish();
  f(true);
  commit();

  A.interact.delay(lastConsensusTime());

  A.publish();
  f(false);
  commit();

  exit();
});
