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

  const r = remote(c, { f: Fun([], Bool) });
  const [rnet, [r1, r2], b] = r.f.withBill([t1, t2]).ALGO({
    simNetRecv: 1,
    simTokensRecv: [2, 3, 4], // incorrect length
    simReturnVal: true,
  })();
  enforce(b);
  enforce(rnet >= 1);
  enforce(r1 >= 2);
  enforce(r2 >= 3);
  transfer([balance(), [balance(t1), t1], [balance(t2), t2]]).to(P);
  commit();
});
