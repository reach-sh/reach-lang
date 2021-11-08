'reach 0.1';


export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
  });
  deploy();

  const f = (() => {
    Alice.publish();
    return false;
  });

  Alice.publish();
  var keepGoing = true;
  invariant(balance() == 0);
  while (keepGoing) {
    commit();
    keepGoing = f();
    continue;
  };
  commit();
});
