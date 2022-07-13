'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    getTokens: Fun([], Array(Token, 5))
  });
  init();

  A.only(() => {
    const toks = declassify(interact.getTokens()); });
  A.publish(toks)
   .check(() => { check(distinct(...toks)) });
  toks.forEach(Token.track);
  commit();

  A.pay([ [1, toks[0]]
        , [1, toks[1]]
        , [1, toks[2]]
        , [1, toks[3]]
        , [1, toks[4]]
        ]);
  commit();

  A.publish();
  transfer(1, toks[0]).to(A);
  transfer(1, toks[1]).to(A);
  transfer(1, toks[2]).to(A);
  // transfer(1, toks[3]).to(A);
  transfer(1, toks[4]).to(A);
  commit();

  exit();
});
