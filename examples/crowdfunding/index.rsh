'reach 0.1';

const Song = Tuple(Bytes(64), Bytes(12), UInt) // Title, id, Price

const crowfundingDeadline = 8;

export const main = Reach.App(() => {
  const A = Participant('Artist', {
    ...hasRandom,
    goal: UInt,
    logAmount: Fun([UInt], Null),
  });

  const F = API('Fan', {
    pickPerk: Fun([Song], Null),
  });

  init();
  
  A.publish();
  commit();

  A.only(() => {
    const goal = declassify(interact.goal);
  });

  A.publish(goal);

  const [ timeRemaining, keepGoing ] = makeDeadline(crowfundingDeadline);

  var totalPool = 0;
  invariant(balance() == totalPool);
  while (keepGoing()) {
    commit();
    try {
      const [ pickedPerk, setPerksParams ] = call(F.pickPerk).pay(
        (picked) => picked[2]).throwTimeout(timeRemaining());
        
      setPerksParams(null);
      totalPool = totalPool + pickedPerk[0][2];

      continue;
    } catch (_) {
      Anybody.publish(); 
      continue;
    }
  }

  transfer([balance()]).to(A);
  commit();

  A.interact.logAmount(balance());
  A.interact.logAmount(totalPool);

  exit();
});

