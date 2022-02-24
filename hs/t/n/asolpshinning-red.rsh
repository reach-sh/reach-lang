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
  const x = parallelReduce(0)
    .invariant(true)
    .while(true)
    .api(P.f,
      (y) => { return [0, [y, tok]]; },
      (y, k) => {
        k(true);
        return x + y;
      })
    .api(P.unf,
      (y) => { return [0]; },
      (y, k) => {
        k(true)
        return x - y;
      })
    .timeout(false);
  commit();
});
