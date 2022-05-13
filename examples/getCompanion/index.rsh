'reach 0.1';

const VI = {
  c: Fun([], Contract),
};
const PI = {
  f: Fun([], Null),
};
const makeServer = (connectors, f) => Reach.App(() => {
  setOptions({connectors});
  const N = 64;
  const A = Participant('A', {
    x: UInt,
    ready: Fun([Contract], Null),
  });
  const V = View(VI);
  const P = API(PI);
  init();

  A.only(() => {
    const ax = declassify(interact.x);
    const xs = Array.replicate(N, ax);
  });
  A.publish(ax, xs);
  A.interact.ready(getContract());
  V.c.set(f);
  commit();
  const [ [], k ] = call(P.f);
  xs.forEach((x) => check(x == ax));
  k(null);
  commit();
  A.publish();
  commit();
  exit();
});
export const serverMay = makeServer(
  [ ALGO, ETH ],
  () => fromSome(getCompanion(), getContract())
);
/*
export const serverMust = makeServer(
  [ ALGO ],
  () => getCompanion(true)
);
*/

export const client = Reach.App(() => {
  const A = Participant('A', {
    r: Contract,
  });
  init();
  A.only(() => {
    const r = declassify(interact.r);
  });
  A.publish(r);
  const ro = remote(r, { ...VI, ...PI });
  const c = ro.c();
  commit();
  A.publish();
  const _ = ro.f.ALGO({
    fees: 2,
    apps: [ c ],
  })();
  commit();
  exit();
});
