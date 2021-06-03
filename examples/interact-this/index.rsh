'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  const Bob   = Participant('Bob', { show: Fun(true, Null) });
  deploy();

  Alice.publish();
  commit();

  Bob.only(() => assume(this != Alice));
  Bob.publish();
  require(Bob != Alice);
  commit();

  Alice.publish();
  Bob.interact.show(this);

  commit();
});
