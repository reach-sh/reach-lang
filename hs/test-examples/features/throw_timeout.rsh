'reach 0.1';

const common = {
  go: Fun([], Bool),
  show: Fun([UInt], Null),
}

export const main = Reach.App(
  {},
  [
    Participant('Alice', { ...common, aliceWager: UInt, bobWager: UInt }),
    Participant('Bob', common)],
  (Alice, Bob) => {
    Alice.only(() => {
      const aWager = declassify(interact.aliceWager);
      const bWager = declassify(interact.bobWager);
    })
    Alice.publish(aWager, bWager);
    commit();
    Bob.publish();

    const [ timeRemaining, keepGoing ] = makeDeadline(10);

    try {
      const [ aliceGone, bobGone, x ] =
        parallelReduce([ false, false, 0 ])
          .invariant(balance() == balance())
          .while(keepGoing() || (!aliceGone && !bobGone))
          .case(Alice,
            (() => ({ when: declassify(interact.go())  })),
            (() => aWager),
            (() => { return [ true, bobGone, x + 1 ]; })
          )
          .case(Bob,
            (() => ({ when: declassify(interact.go()) })),
            (() => bWager),
            (() => { return [ aliceGone, true, x + 1 ]; })
          )
          // equivalent to, .timeout(timeRemaining(), () => { throw <lhs>; });
          .throwTimeout(timeRemaining())
      transfer(balance()).to(Alice);
    } catch (e) {
      Anybody.publish();
      transfer(balance()).to(Bob);
    }
    commit();
  }
);
