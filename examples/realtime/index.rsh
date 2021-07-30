'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    t: UInt,
  });
  deploy();

  A.only(() => {
    const t = declassify(interact.t); });
  A.publish(t); commit();

  const aStep = (lab, tN) => {
    const entry = (step) =>
      [ step, lab, tN,
        lastConsensusTime(),
        lastConsensusSecs() ];
    A.interact.log(entry('before wait'));
    wait(tN);
    A.interact.log(entry('after wait'));
    A.publish()
     .timeout(tN, () => {
       A.interact.log(entry('in timeout'));
       A.publish();
       commit();
       A.interact.log(entry('after timeout'));
       exit();
     });
    commit();
    A.interact.log(entry('after commit'));
  };

  aStep('default (relativeBlocks)', t);
  aStep('relativeBlocks', relativeTime(t));
  aStep('absoluteBlocks', absoluteTime(lastConsensusTime() + t));
  aStep('relativeSecs', relativeSecs(t));
  aStep('absoluteSecs', absoluteSecs(lastConsensusSecs() + t));

  exit();
});
