'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    cost: UInt,
    token: Token,
  });
  deploy();

  A.only(() => {
    const token = declassify(interact.token);
    const cost = declassify(interact.cost);
  });
  A.publish(cost, token).pay(cost);
  assert(balance() == cost);
  lock(cost);
  assert(balance() == 0);
  commit();

  A.pay([ [cost, token] ]);
  assert(balance(token) == cost);
  lock(cost, token);
  assert(balance(token) == 0);
  commit();

});
