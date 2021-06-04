'reach 0.1';
'use strict';

// Protocol
const DELAY = 10; // in blocks
const [ isOutcome, A_TIMEOUT, B_TIMEOUT, A_WON, B_WON ] = makeEnum(4);

const Player =
      { ...hasRandom,
        getMove: Fun([UInt, UInt], Tuple(Bool, UInt)),
        showOutcome: Fun([UInt], Null) };
const Alice =
      { ...Player,
        getParams: Fun([], Tuple(UInt, UInt)) };
const Bob =
      { ...Player,
        acceptParams: Fun([UInt, UInt], Null) };

export const main =
  Reach.App(
    {},
    [Participant('A', Alice), Participant('B', Bob)],
    (A, B) => {
      function sendOutcome(which) {
        return () => {
          each([A, B], () => {
            interact.showOutcome(which); }); }; }

      A.only(() => {
        const [ wagerAmount, initialHeap ] = declassify(interact.getParams());
        const _coinFlipA = interact.random();
        const commitA = declassify(digest(_coinFlipA));});
      A.publish(wagerAmount, initialHeap, commitA)
        .pay(wagerAmount);
      commit();

      B.only(() => {
        interact.acceptParams(wagerAmount, initialHeap);
        const coinFlipB = declassify(interact.random()); });
      B.publish(coinFlipB)
        .pay(wagerAmount)
        .timeout(DELAY, () => closeTo(A, sendOutcome(B_TIMEOUT)));
      commit();

      A.only(() => {
        const coinFlipA = declassify(_coinFlipA); });
      A.publish(coinFlipA)
        .timeout(DELAY, () => {
          closeTo(B, sendOutcome(A_TIMEOUT));
        });
      require(commitA == digest(coinFlipA));
      const AisFirst = (((coinFlipA % 2) + (coinFlipB % 2)) % 2) == 0;

      var [ AsTurn, heap1, heap2 ] = [ AisFirst, initialHeap, initialHeap ];
      invariant(balance() == (2 * wagerAmount));
      while ( heap1 + heap2 > 0 ) {

        const applyMove = (choose1, amount) => {
          require(amount <= (choose1 ? heap1 : heap2));
          if ( choose1 ) {
            return [ !AsTurn, heap1 - amount, heap2 ];
          } else {
            return [ !AsTurn, heap1, heap2 - amount ]; } };

        if ( AsTurn ) {
          commit();

          A.only(() => {
            const _move = interact.getMove(heap1, heap2);
            const [ choose1, amount ] = declassify(_move);
            assume(amount <= (choose1 ? heap1 : heap2)); });
          A.publish(choose1, amount)
            .timeout(DELAY, () => closeTo(B, sendOutcome(A_TIMEOUT)));

          [ AsTurn, heap1, heap2 ] = applyMove(choose1, amount);
          continue;
        } else {
          commit();

          B.only(() => {
            const _move = interact.getMove(heap1, heap2);
            const [ choose1, amount ] = declassify(_move);
            assume(amount <= (choose1 ? heap1 : heap2)); });
          B.publish(choose1, amount)
            .timeout(DELAY, () => closeTo(A, sendOutcome(B_TIMEOUT)));

          [ AsTurn, heap1, heap2 ] = applyMove(choose1, amount);
          continue; } }

      const [ toA, toB ] = AsTurn ? [ 2, 0 ] : [ 0, 2 ];
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      const outcome = AsTurn ? A_WON : B_WON;
      commit();

      sendOutcome(outcome)(); } );
