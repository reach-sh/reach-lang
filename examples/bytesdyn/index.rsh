'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ connectors: [ ETH ] });
  const A = Participant('A', { t: BytesDyn });
  const B = Participant('B', { chk: Fun([BytesDyn], Null) });
  const E = Events({ u: [BytesDyn, Digest] });
  init();
  A.only(() => { const t = declassify(interact.t); });
  A.publish(t);
  const u = digest(t);
  E.u(t, u);
  B.interact.chk(t);
  commit();
  A.only(() => { const t2 = t; });
  A.publish(t2);
  check(t == t2);
  const u2 = digest(t2);
  check(u == u2);
  commit();
  A.only(() => { const v = digest(t); });
  A.publish(v);
  check(u == v);
  commit();

  const go = (f, g) => {
    A.only(() => { const x = f(t); });
    A.publish(x);
    B.interact.chk(g(x));
    commit();
  };
  go((x) => [ 1, true, x, 3 ],
     (o) => o[2]);
  go((x) => Array.replicate(3, x),
     (o) => o[2]);

  exit();
});
