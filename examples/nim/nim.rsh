'reach 0.1 exe';

// Game
function applyMove(AsTurn, heap1, heap2, choose1, amount) {
  require(amount <= (choose1 ? heap1 : heap2))
  if ( choose1 ) {
    return [ !AsTurn, heap1 - amount, heap2 ];
  } else {
    return [ !AsTurn, heap1, heap2 - amount ]; } }

function closeTo(Who, result) {
  return (() => {
    Who.publish();
    transfer(balance()).to(Who);
    commit();
    return result; }); }

// Protocol
const A = participant({});
const B = participant({});
const DELAY = 10; // in blocks

function main() {
  A.only(() => {
    const wagerAmount = declassify(is(uint256, interact.getWagerAmount()));
    const initialHeap = declassify(is(uint256, interact.getInitialHeap()));
    const _coinFlipA = random();
    const commitA = declassify(digest(_coinFlipA));});
  A.publish(wagerAmount, initialHeap, commitA)
    .pay(wagerAmount)
  commit();

  B.only(() => {
    interact.acceptWager(wagerAmount, initialHeap);
    const coinFlipB = declassify(random()); });
  B.publish(coinFlipB)
    .pay(wagerAmount)
    .timeout(DELAY, closeTo(A, "B never accepted"));
  commit();

  A.only(() => {
    const coinFlipA = declassify(_coinFlipA); });
  A.publish(coinFlipA)
    .timeout(DELAY, () => {
      B.publish();
      transfer(balance()).to(B);
      commit();
      return "A never revealed coinflip"; });
  require(commitA == digest(coinFlipA));
  const AisFirst = (( coinFlipA + coinFlipB ) % 2) == 0;
  
  var [ AsTurn, heap1, heap2 ] = [ AisFirst, initialHeap, initialHeap ];
  invariant(balance() == (2 * wagerAmount));
  while ( heap1 + heap2 > 0 ) {
    if ( AsTurn ) {
      commit();
      
      A.only(() => {
        const choose1 = declassify(is(bool,interact.getHeap(heap1, heap2)));
        const amount = declassify(is(uint256,interact.getAmount(heap1, heap2)));
        assume(amount <= (choose1 ? heap1 : heap2)); });
      A.publish(choose1, amount)
        .timeout(DELAY, () => {
          B.publish();
          transfer(balance()).to(B);
          commit();
          return "A timed out move"; });
      
      [ AsTurn, heap1, heap2 ] = applyMove(AsTurn, heap1, heap2, choose1, amount);
      continue;
    } else {
      commit();
      
      B.only(() => {
        const choose1 = declassify(is(bool,interact.getHeap(heap1, heap2)));
        const amount = declassify(is(uint256,interact.getAmount(heap1, heap2)));
        assume(amount <= (choose1 ? heap1 : heap2)); });
      B.publish(choose1, amount)
        .timeout(DELAY, () => {
          A.publish();
          transfer(balance()).to(A);
          commit();
          return "B timed out move"; });
      
      [ AsTurn, heap1, heap2 ] = applyMove(AsTurn, heap1, heap2, choose1, amount);
      continue; } }

  const [ toA, toB ] = AsTurn ? [ 2, 0 ] : [ 0, 2 ];
  transfer(toA * wagerAmount).to(A);
  transfer(toB * wagerAmount).to(B);
  commit();

  interact.showOutcome(AsTurn ? "A won" : "B won");
  return "Game is over"; }
