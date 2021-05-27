'reach 0.1';
'use strict';

function winner (countA, countB) {
  if ( countA > countB ) {
    return 2;
  } else if ( countB > countA ) {
    return 1;
  }
  return 0;
};

assert(winner(1, 0) == 2);
assert(winner(0, 1) == 1);
assert(winner(1, 1) == 0);

forall(UInt, (countA) =>
  forall(UInt, (countB) =>
    assert(winner(countA, countB) < 3)));

const common = {
  count: UInt,
};

export const main = Reach.App(() => {
  const A = Participant('Alice', { ...common, amount: UInt });
  const B = Participant('Bob', common);
  deploy();

  A.only(() => {
    const countA = declassify(interact.count);
    const amount = declassify(interact.amount); });
  A.publish(amount, countA)
   .pay(amount);
  commit();

  B.only(() => {
    const countB = declassify(interact.count); });
  B.publish(countB)
   .pay(amount);

  const outcome = winner(countA, countB);
  const [ toA, toB ] =
    outcome == 2 ? [ 2, 0 ] :
    outcome == 1 ? [ 0, 2 ] :
    /**/           [ 1, 1 ];
  transfer(toA * amount).to(A);
  transfer(toB * amount).to(B);
  commit();

  exit();
});


