'reach 0.1';

export const a = Reach.App(() => {
  const P = Participant("P", { c: Contract });
  init();

  P.only(() => { const c = declassify(interact.c); })
  P.publish(c);
  const r = remote(c, { f: Fun([], Null) });
  r.f.ALGO({
    simNetRecv: 123,
    simReturnVal: null
  })();
  commit();
});
