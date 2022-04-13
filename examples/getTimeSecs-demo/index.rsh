'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    giveTime: Fun([UInt], Null),
  });
  init();
  A.publish();
  commit();
  A.interact.giveTime(lastConsensusTime());
  exit();
});
