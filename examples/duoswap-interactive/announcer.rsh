'reach 0.1';

export const Common = {
  ...hasConsoleLogger,
  hear: Fun([Address], Null),
};

export const main = Reach.App(() => {
  const Manager = Participant('Manager', {
    ...Common,
    printInfo: Fun([], Null),
  });
  const Listener = ParticipantClass('Listener', {
    ...Common,
    getPoolInfo: Fun([], Tuple(Bool, Address)),
  });

  deploy();

  Manager.publish();
  Manager.interact.printInfo();

  var [] = [];
  invariant(balance() == 0);
  while (true) {
    commit();

    Listener.only(() => {
      const [ when, poolInfo ] = declassify(interact.getPoolInfo());
    });

    Listener.publish(poolInfo)
      .when(when)
      .timeout(false);

    Manager.interact.hear(poolInfo);
    Listener.interact.hear(poolInfo);

    commit();

    // each([ Manager, Listener ], () => {
    //   interact.hear(poolInfo) });

    Manager.publish();

    continue;
  }

  commit();
  exit();

});
