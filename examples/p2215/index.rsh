'reach 0.1';

const P_int = {
  f: Fun([UInt], UInt),
};

export const mainS = Reach.App(() => {
  const D = Participant('D', {
    ready: Fun([Contract], Null),
  });
  const P = API(P_int);
  init();
  D.publish();
  D.interact.ready(getContract());
  commit();

  const [ [fd], fk ] = call(P.f);
  fk(fd + 1);
  commit();

  D.publish();
  commit();
  exit();
});

export const mainC = Reach.App(() => {
  const D = Participant('D', {
    serverInfo: Contract,
    log: Fun(true, Null),
  });
  init();
  D.only(() => {
    const serverInfo = declassify(interact.serverInfo);
  });
  D.publish(serverInfo);
  const server = remote(serverInfo, P_int);

  var x = 0;
  invariant(balance() == 0);
  while ( x == 0 ) {
    commit();
    D.publish();
    x = x+1;
    continue;
  }
  const fr = server.f(x);
  D.interact.log(fr);
  commit();

  exit();
});
