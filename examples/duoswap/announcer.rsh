'reach 0.1';

const Addr = Bytes(64);

export const Common = {
  hear: Fun([Addr, Bool], Null),
};

export const main = Reach.App(() => {
  const Manager = Participant('Manager', {
    ...Common,
    printInfo: Fun([], Null),
    getPoolInfo: Fun([], Tuple(Addr, Bool)),
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
      const [poolInfo, usesNetwork] = declassify(interact.getPoolInfo());
    });

    Manager
      .publish(poolInfo, usesNetwork)
      .timeout(false);

    Manager.interact.hear(poolInfo, usesNetwork);
    Listener.interact.hear(poolInfo, usesNetwork);

    commit();

    Manager.publish();

    continue;
  }

  commit();
  exit();

});
