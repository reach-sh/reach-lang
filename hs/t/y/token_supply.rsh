'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  deploy();
  A.publish();
  const t = new Token({ supply: 10 });
  assert(t.supply() == 10);
  t.burn(2);
  assert(Token.supply(t) == 8);
  t.burn(8);
  t.destroy();
  commit();
});
