'reach 0.1';

const f = ((p) => {
  p.only(() => {
    const choice = declassify(interact.choice)
  });
  p.publish(choice);
});

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    choice: UInt,
  });
  deploy();
  f(Alice);
  var keepGoing = true;
  invariant(keepGoing);
  while (keepGoing) {
    commit();
    f(Alice)
    continue;
  };
});
