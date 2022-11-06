'reach 0.1';

export const veriC = Reach.App(() => {
  setOptions({ verifyArithmetic: true });
  const A = Participant('A', { x: UInt256 });
  init();
  A.only(() => {
    const x = declassify(interact.x);
    check(x < UInt256(UInt.max));
  });
  A.publish(x);
  check(x < UInt256(UInt.max));
  const y = UInt(x);
  commit();
  A.only(() => {
    const z = y;
  });
  A.publish(z);
  check(z == y);
  commit();
  exit();
});

const I = {
  add: Fun([UInt256, UInt256], UInt256),
  sub: Fun([UInt256, UInt256], UInt256),
  mul: Fun([UInt256, UInt256], UInt256),
  div: Fun([UInt256, UInt256], UInt256),
  mod: Fun([UInt256, UInt256], UInt256),
  cast: Fun([UInt256], UInt),
};

export const trapS = Reach.App(() => {
  const A = Participant('A', { go: Fun([Contract], Null) });
  const P = API(I);
  init();
  A.publish();
  A.interact.go(getContract());
  commit();
  fork()
    .api(P.add, (x, y, k) => { k(x+y); })
    .api(P.sub, (x, y, k) => { k(x-y); })
    .api(P.mul, (x, y, k) => { k(x*y); })
    .api(P.div, (x, y, k) => { k(x/y); })
    .api(P.mod, (x, y, k) => { k(x%y); })
    .api(P.cast, (x, k) => { k(UInt(x)); })
  ;
  commit();
  A.publish();
  commit();
  exit();
});

export const trapC = Reach.App(() => {
  const A = Participant('A', { server: Contract, go: Fun([], Null) });
  const P = API(I);
  init();
  A.only(() => {
    const server = declassify(interact.server);
  });
  A.publish(server);
  const r = remote(server, I);
  A.interact.go();
  commit();
  fork()
    .api(P.add, (x, y, k) => { k(r.add(x,y)); })
    .api(P.sub, (x, y, k) => { k(r.sub(x,y)); })
    .api(P.mul, (x, y, k) => { k(r.mul(x,y)); })
    .api(P.div, (x, y, k) => { k(r.div(x,y)); })
    .api(P.mod, (x, y, k) => { k(r.mod(x,y)); })
    .api(P.cast, (x, k) => { k(r.cast(x)); })
  ;
  commit();
  exit();
});

const makeMain = (which) => Reach.App(() => {
  const A = Participant('A', {
    vs1: Tuple(UInt256, UInt256, UInt, UInt),
    check: Fun(true, Null),
  });
  init();

  A.only(() => {
    const vs1 = declassify(interact.vs1);
  });
  A.publish(vs1);
  const [b1, b2, s1, s2] = vs1;

  const b3 = UInt256(1);
  const s3 = 1;
  const s1b = UInt256(s1);
  const s2b = UInt256(s2);
  const b2s = UInt(b2);

  const f = (isN, g) => [
    g(b1, b2),
    g(b1, b3),
    g(b2, b3),
    g(s1, s2),
    g(s1, s3),
    g(s1, b2s),
    g(s2, s3),
    g(s1b, s2b),
    g(s1b, b3),
    ...(isN ? [
      UInt(g(s1b, s2b)),
      UInt(g(s1b, b3)),
    ] : []),
  ];
  commit();

  const F = (isN) => (i, g) => {
    A.publish();
    const x1 = f(isN, g);
    commit();
    A.only(() => {
      const x2 = x1;
    });
    A.publish(x2);
    check(x2 == x1);
    commit();
    A.interact.check(i, x1);
  };
  const G = F(false);
  const H = F(true);

  if ( which ) {
  H( 0, (u, v) => u + v);
  H( 1, (u, v) => u - v);
  H( 2, (u, v) => u * v);
  H( 3, (u, v) => u / v);
  H( 4, (u, v) => u % v);
  H( 5, (u, v) => u ^ v);
  H( 6, (u, v) => u | v);
  } else {
  H( 7, (u, v) => u & v);
  G( 8, (u, v) => u < v);
  G( 9, (u, v) => u <= v);
  G(10, (u, v) => u == v);
  G(11, (u, v) => u >= v);
  G(12, (u, v) => u > v);
  }

  exit();
});

export const main1 = makeMain(true);
export const main2 = makeMain(false);
