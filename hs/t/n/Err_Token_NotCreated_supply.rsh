'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    tok: Token,
  });
  init();
  A.only(() => {
    const tok = declassify(interact.tok); });
  A.publish(tok);
  const x = tok.supply();
  commit();
  exit();
});
