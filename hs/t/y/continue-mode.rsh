'reach 0.1';


export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    choice: UInt,
  });
  deploy();

  const f = (() => {
    Alice.publish();
    return false;
  });

  Alice.publish();
  var keepGoing = true;
  invariant(keepGoing);
  while (keepGoing) {
    commit();
    keepGoing = f();
    continue;
  };
  commit();
});
