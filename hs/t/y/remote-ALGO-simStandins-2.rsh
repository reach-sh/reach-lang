'reach 0.1';

export const a = Reach.App(() => {
  const P = Participant("P", {
    c: Contract,
    t1: Token,
    t2: Token,
  });
  init();

  P.only(() => {
    const c = declassify(interact.c);
    const t1 = declassify(interact.t1);
    const t2 = declassify(interact.t2);
    assume(t1 != t2);
  })
  P.publish(c, t1, t2);
  assert(t1 != t2);
  const r = remote(c, { f: Fun([], UInt) });
  const n = r.f.bill([1, [1, t1], [1, t2]]).ALGO({
    simNetRecv: 1,
    simTokensRecv: [2, 3],
    simReturnVal: 4,
  })();
  transfer([1, [1, t1], [1, t2]]).to(P);
  commit();
});
