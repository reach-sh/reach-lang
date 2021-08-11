'reach 0.1';

// this returns the same primitive and should be simplified in the .dl and not returned as separate variables
const f = (wager) => {
    if (wager > 0) { return 2 } else { return 2 };
};

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    wager: UInt,
  });
  const Bob   = Participant('Bob', {
    acceptWager: Fun([UInt], Null),
  });
  deploy();

  Alice.only(() => {
    const wager = declassify( f(interact.wager));
  });
  Alice.publish(wager)
  commit();

  Bob.only(() => {
    const really = declassify(interact.acceptWager(f(wager)));
  });
  Bob.publish(really)
  commit();
});
