'reach 0.1';

// Protocol
const DELAY = 10; // in blocks

const Player =
      { getMove: Fun([UInt256, UInt256], Array(Bool, UInt256)),
        showOutcome: Fun([Bytes], Null) };
const Alice =
      { ...Player,
        getParams: Fun([], Array(UInt256, UInt256)) };
const Bob =
      { ...Player,
        acceptParams: Fun([UInt256, UInt256], Null) };

export const main =
  Reach.App(
    {},
    [["A", Alice], ["B", Bob]],
    function (A, B) {
      A.only(() => {
        const [ wagerAmount, initialHeap ] = declassify(interact.getParams());
        const _coinFlipA = random();
        const commitA = declassify(digest(_coinFlipA));});
      A.publish(wagerAmount, initialHeap, commitA)
        .pay(wagerAmount)
      commit();

      B.only(() => {
        interact.acceptParams(wagerAmount, initialHeap);
        const coinFlipB = declassify(random()); });
      B.publish(coinFlipB)
        .pay(wagerAmount)
        .timeout(DELAY, closeTo(A, "B never accepted"));
      commit();

      A.only(() => {
        const coinFlipA = declassify(_coinFlipA); });
      A.publish(coinFlipA)
        .timeout(DELAY, closeTo(B, "A never revealed coinflip"));
      require(commitA == digest(coinFlipA));
      const AisFirst = (( coinFlipA + coinFlipB ) % 2) == 0;

      var [ AsTurn, heap1, heap2 ] = [ AisFirst, initialHeap, initialHeap ];
      invariant(balance() == (2 * wagerAmount));
      while ( heap1 + heap2 > 0 ) {

        function applyMove(choose1, amount) {
          require(amount <= (choose1 ? heap1 : heap2))
          if ( choose1 ) {
            return [ !AsTurn, heap1 - amount, heap2 ];
          } else {
            return [ !AsTurn, heap1, heap2 - amount ]; } }

        if ( AsTurn ) {
          commit();

          A.only(() => {
            const [ choose1, amount ]  = declassify(interact.getMove(heap1, heap2));
            assume(amount <= (choose1 ? heap1 : heap2)); });
          A.publish(choose1, amount)
            .timeout(DELAY, closeTo(B, "A timed out move"));

          [ AsTurn, heap1, heap2 ] = applyMove(choose1, amount);
          continue;
        } else {
          commit();

          B.only(() => {
            const [ choose1, amount ]  = declassify(interact.getMove(heap1, heap2));
            assume(amount <= (choose1 ? heap1 : heap2)); });
          B.publish(choose1, amount)
            .timeout(DELAY, closeTo(A, "B timed out move"));

          [ AsTurn, heap1, heap2 ] = applyMove(choose1, amount);
          continue; } }

      const [ toA, toB ] = AsTurn ? [ 2, 0 ] : [ 0, 2 ];
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      const outcome = AsTurn ? "A won" : "B won";
      commit();

      A.only(() => {
        interact.showOutcome(outcome); });
      B.only(() => {
        interact.showOutcome(outcome); });
      return "Game is over"; } );
