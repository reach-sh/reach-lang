'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant("A", { t: Token, b: Bool });
  setOptions({ autoTrackPublishedTokens: false });
  init();
  A.only(() => { const t = declassify(interact.t);
                 const b = declassify(interact.b); });
  A.publish(t, b);
  if (b) { Token.track(t); }
  commit();
  exit();
});
