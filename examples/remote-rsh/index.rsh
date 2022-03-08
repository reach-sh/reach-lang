'reach 0.1';

const P_int = {
  f: Fun([UInt], UInt),
  g: Fun([UInt], UInt),
  h: Fun([UInt], UInt),
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

  D.publish();
  transfer([ gd, [ hd, tok ]]).to(D);
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
    inform: Fun([UInt, UInt, UInt, UInt], Null),
  });
  init();
  D.only(() => {
    const serverInfo = declassify(interact.serverInfo);
  });
  D.publish(serverInfo);
  const server = remote(serverInfo, V_int);
  const x = server.x();
  const serverTok = server.tok();
  const fr = server.f(x);
  commit();
  D.only(() => {
    const tok = serverTok;
  });
  D.publish(tok).pay(fr);
  check(tok == serverTok);
  const gr = server.g.pay(fr)(fr);
  const gamt = [gr, tok];
  commit();
  D.pay([gamt]);
  const hr = server.h.pay([gamt])(gr);
  commit();
  D.interact.inform(x, fr, gr, hr);
  exit();
});
