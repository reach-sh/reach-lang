'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...hasConsoleLogger,
    keepGoing: Fun([], Tuple(Bool, UInt))
  });
  const V = View({ currentInt: UInt });
  deploy();

  A.publish();
  V.currentInt.set(1);

  var [keepGoing, curInt] = [true, 1];
  {
    V.currentInt.set(curInt);
  }
  invariant(balance() == 0);
  while ( keepGoing ) {
    commit();

    A.only(() => {
      const [kg, ci] = declassify(interact.keepGoing());
    });
    A.publish(kg, ci);

    [keepGoing, curInt] = [kg, ci];
    continue;
  }

  commit();

});
