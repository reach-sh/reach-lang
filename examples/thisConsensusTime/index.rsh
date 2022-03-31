'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();
  A.publish();
  commit();

  A.publish();

  var [ lastTime, lastSecs, i ] = [ thisConsensusTime(), thisConsensusSecs(), 0 ];
  invariant(lastTime > lastConsensusTime() &&
            lastTime == thisConsensusTime() &&
            lastSecs >= lastConsensusSecs() &&
            balance() == 0);
  while (i < 5) {
    commit();
    A.publish();
    assert(thisConsensusTime() > lastTime);
    [ lastTime, lastSecs, i ] = [ thisConsensusTime(), thisConsensusSecs(), i + 1 ];
    continue;
  }

  commit();
});
