'reach 0.1';
export const main = Reach.App(() => {
  const C = Participant('C', { tok: Token });
  const P = API('P', {
    f: Fun([UInt], Bool),
    unf: Fun([UInt], Bool),
  });
  init();
  C.only(() => { const tok = declassify(interact.tok); });
  C.publish(tok);
  commit();
  fork()
  .api(P.f,
    (y) => { return [0, [y, tok]]; },
    (y, k) => {
      k(true);
      commit();
    })
  .api(P.unf,
    (y) => { return [0]; },
    (y, k) => {
      k(true)
      commit();
    })
  .timeout(false);
});
