'reach 0.1';

const P_int = {
  f: Fun([Address, Contract, Address], Null),
};

export const mainS = Reach.App(() => {
  const D = Participant('D', {
    ...hasConsoleLogger,
    x: UInt,
    ready: Fun([Contract], Null),
  });
  const P = API(P_int);
  init();
  D.only(() => {
    const x = declassify(interact.x);
  });
  D.publish(x).pay(x);
  D.interact.ready(getContract());
  commit();
  const [ [who, ctc, ctca], fk ] = call(P.f)
    .assume((who, ctc, _) => {
      check(Contract.addressEq(ctc, this));
    });
  D.interact.log([ who, ctc, ctca, this, D ]);
  // Force that this must be called by a contract
  check(Contract.addressEq(ctc, this));
  transfer(x).to(who);
  fk(null);
  commit();
  D.publish();
  commit();
  exit();
});

export const mainC = Reach.App(() => {
  const D = Participant('D', {
    ...hasConsoleLogger,
    serverInfo: Contract,
    who: Address,
  });
  init();
  D.only(() => {
    const serverInfo = declassify(interact.serverInfo);
    const who = declassify(interact.who);
  });
  D.publish(serverInfo, who);
  const server = remote(serverInfo, P_int);
  const args = [ who, getContract(), getAddress() ];
  D.interact.log(args, this);
  const x = server.f.ALGO({ fees: 1 })(...args);
  D.interact.log(x);
  commit();
  exit();
});
