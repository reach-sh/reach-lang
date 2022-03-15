'reach 0.1';

const P_int = {
  f: Fun([UInt], UInt),
  g: Fun([UInt], UInt),
  h: Fun([UInt], UInt),
  i: Fun([], Null),
};

export const mainS = Reach.App(() => {
  const D = Participant('D', {
    x: UInt,
    tok: Token,
    ready: Fun([Contract], Null),
  });
  const V = View({
    x: UInt,
    tok: Token,
  });
  const P = API(P_int);
  init();
  D.only(() => {
    const x = declassify(interact.x);
    const tok = declassify(interact.tok);
  });
  D.publish(x, tok);
  V.x.set(x);
  V.tok.set(tok);
  D.interact.ready(getContract());
  commit();

  const [ [fd], fk ] =
    call(P.f)
    .assume((fa) => { check(fa == x); });
  check(fd == x);
  fk(x + 1);
  commit();

  const [ [gd], gk ] =
    call(P.g)
    .assume((gx) => { check(gx == x + 1); })
    .pay((gx) => gx);
  check(gd == x + 1);
  gk(x + 2);
  commit();

  const [ [hd], hk ] =
    call(P.h)
    .assume((hx) => { check(hx == x + 2); })
    .pay((hx) => [[hx, tok]]);
  check(hd == x + 2);
  hk(x + 3);
  commit();

  const [ [], ik ] =
    call(P.i);
  ik(null);
  transfer([gd, [ hd, tok ]]).to(this);
  commit();

  D.publish();
  commit();
  exit();
});

const V_int = {
  x: Fun([], UInt),
  tok: Fun([], Token),
  ...P_int,
};

export const mainC = Reach.App(() => {
  const D = Participant('D', {
    serverInfo: Contract,
    ztok: Token,
    inform: Fun([UInt, UInt, UInt, UInt], Null),
  });
  init();
  D.only(() => {
    const serverInfo = declassify(interact.serverInfo);
    const ztok = declassify(interact.ztok);
  });
  D.publish(serverInfo, ztok);
  const server = remote(serverInfo, V_int);
  const x = server.x();
  const serverTok = server.tok();
  const fr = server.f(x);
  commit();
  D.only(() => {
    const tok = serverTok;
    check(ztok != tok);
  });
  D.publish(tok).pay(fr);
  check(ztok != tok);
  check(tok == serverTok);
  const gr = server.g.pay(fr)(fr);
  const gamt = [gr, tok];
  commit();
  D.pay([gamt]);
  const hr = server.h.pay([gamt])(gr);
  commit();
  D.publish();
  server.i.bill([fr, gamt])();
  transfer([fr, gamt]).to(D);
  commit();
  D.interact.inform(x, fr, gr, hr);
  exit();
});
