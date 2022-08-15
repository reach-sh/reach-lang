'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant("A", { t: Token });
  setOptions({ autoTrackPublishedTokens: false });
  init();
  A.only(() => { const t = declassify(interact.t); });
  A.publish(t);
  commit();
  const bal = balance(t);
  exit();
});
