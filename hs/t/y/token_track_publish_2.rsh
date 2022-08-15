'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant("A", { ts: Tuple(Token, Token) });
  setOptions({ autoTrackPublishedTokens: false });
  init();
  A.only(() => { const [t1, t2] = declassify(interact.ts); });
  A.publish(t1, t2);
  Token.track(t2);
  commit();
  const bal = balance(t2);
  exit();
});
