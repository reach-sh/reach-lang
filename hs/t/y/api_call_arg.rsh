'reach 0.1';

const Addr = Bytes(64);

export const main = Reach.App(() => {
  const Constructor = Participant('Constructor', {
    printInfo: Fun([], Null),
  });
  const Manager = API('Manager', {
    getPoolInfo: Fun([Addr], Null),
  });
  const Listener = ParticipantClass('Listener', {
    hear: Fun([Addr], Null),
  });

  deploy();

  Constructor.publish();
  Constructor.interact.printInfo();

  var [] = [];
  invariant(balance() == 0);
  while (true) {
    commit();

    const [ [poolInfo], k ] = call(Manager.getPoolInfo).throwTimeout(false);
    k(null);

    Listener.interact.hear(poolInfo);

    continue;
  }

  commit();
  exit();

});
