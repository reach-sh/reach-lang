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
      [ ...step, lab,
        lastConsensusTime(),
        lastConsensusSecs() ];
    const wt = tN();
    A.interact.log(entry(['before wait', wt]));
    wait(wt);
    const tt = tN();
    A.interact.log(entry(['after wait', wt, tt]));
    A.publish()
     /*.timeout(tt, () => {
       A.interact.log(entry(['in timeout', wt, tt]));
       A.publish();
       commit();
       A.interact.log(entry(['after timeout', wt, tt]));
       exit();
     })*/;
    commit();
    A.interact.log(entry(['after commit', wt, tt]));
  };

  aStep('default (relativeBlocks)', () => t);
  aStep('relativeBlocks', () => relativeTime(t));
  aStep('absoluteBlocks', () => absoluteTime(lastConsensusTime() + t));
  aStep('relativeSecs', () => relativeSecs(t));
  aStep('absoluteSecs', () => absoluteSecs(lastConsensusSecs() + t));

  exit();
});
