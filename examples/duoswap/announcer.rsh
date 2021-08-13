'reach 0.1';

const Addr = Bytes(64);

export const Common = {
  hear: Fun([Addr], Null),
};

export const main = Reach.App(() => {
  const Manager = Participant('Manager', {
    ...Common,
    printInfo: Fun([], Null),
    getPoolInfo: Fun([], Addr),
  });
  const Listener = ParticipantClass('Listener', {
    ...Common,
  });

  deploy();

  Manager.publish();
  Manager.interact.printInfo();

  var [] = [];
  invariant(balance() == 0);
  while (true) {
    commit();

    Manager.only(() => {
      const poolInfo = declassify(interact.getPoolInfo());
    });

    Manager
      .publish(poolInfo)
      .timeout(false);

    Manager.interact.hear(poolInfo);
    Listener.interact.hear(poolInfo);

    commit();

    Manager.publish();

    continue;
  }

  commit();
  exit();

});
