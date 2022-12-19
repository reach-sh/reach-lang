'reach 0.1';

const P_int = {
  h: Fun([], Null),
};

export const mainS = Reach.App(() => {
  const D = Participant('D', {
    x: UInt,
    tok1: Token,
    tok2: Token,
    ready: Fun([Contract], Null),
  });
  const P = API(P_int);
  init();
  D.only(() => {
    const x = declassify(interact.x);
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    assume(distinct(tok1, tok2));
  });
  D.publish(x, tok1, tok2);
  D.interact.ready(getContract());
  commit();
  const [ [], hk ] =
    call(P.h)
    .pay(() => [x, [x, tok1], [x, tok2]]);
  hk(null);
  commit();
  D.publish();
  transfer([x, [x, tok1], [x, tok2]]).to(this);
  commit();
  exit();
});

const makeMainC = (txnOrderForward) => Reach.App(() => {
  const D = Participant('D', {
    serverInfo: Contract,
    x: UInt,
    tok1: Token,
    tok2: Token,
    inform: Fun(true, Null),
  });
  init();
  D.only(() => {
    const serverInfo = declassify(interact.serverInfo);
    const x = declassify(interact.x);
    const tok1 = declassify(interact.tok1);
    const tok2 = declassify(interact.tok2);
    assume(distinct(tok1, tok2));
  });
  D.publish(serverInfo, x, tok1, tok2);
  const server = remote(serverInfo, P_int);
  commit();
  D.pay([x, [x, tok1], [x, tok2]]);
  const hr = server.h
    .ALGO({txnOrderForward})
    .pay([x, [x, tok1], [x, tok2]])
    ();
  commit();
  D.interact.inform(hr);
  exit();
});

export const mainC1 = makeMainC(false);
export const mainC2 = makeMainC(true);
