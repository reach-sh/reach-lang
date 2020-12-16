'reach 0.1';

// Protocol
const DELAY = 20; // in blocks
const howMany = 2;

const Player =
      { ...hasRandom,
        getMove: Fun([Array(UInt, 2)], Tuple(UInt, UInt)),
        showOutcome: Fun([Bytes(64)], Null) };
const Alice =
      { ...Player,
        getParams: Fun([], Tuple(UInt, UInt)) };
const Bob =
      { ...Player,
        acceptParams: Fun([UInt, UInt], Null) };

export const main =
  Reach.App(
    {},
    [['A', Alice], ['B', Bob]],
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
        .timeout(DELAY, () => closeTo(A, sendOutcome('B never accepted')));
      commit();

      A.only(() => {
        const coinFlipA = declassify(_coinFlipA); });
      A.publish(coinFlipA)
        .timeout(DELAY, () => {
          closeTo(B, sendOutcome('A never revealed coinflip'));
        });
      require(commitA == digest(coinFlipA));
      const AisFirst = (((coinFlipA % 2) + (coinFlipB % 2)) % 2) == 0;

      var [ AsTurn, heaps ] =
        [ AisFirst, Array.iota(howMany).map(x => initialHeap) ];
      invariant(balance() == (2 * wagerAmount));
      while ( heaps.reduce(0, add) > 0 ) {
        const doMove = (now, next) => {
          now.only(() => {
            const _move = interact.getMove(heaps);
            const [ choice, amount ] = declassify(_move);
            assume(choice < howMany);
            assume(amount <= heaps[choice]); });
          now.publish(choice, amount)
            .timeout(DELAY, () =>
              closeTo(next, sendOutcome('timed out move')));

          require(choice < howMany);
          require(amount <= heaps[choice]);
          return [ !AsTurn, heaps.set(choice, heaps[choice] - amount) ]; };

        if ( AsTurn ) {
          commit();
          const nextSt = doMove(A, B);
          [ AsTurn, heaps ] = nextSt;
          continue;
        } else {
          commit();
          const nextSt = doMove(B, A);
          [ AsTurn, heaps ] = nextSt;
          continue; } }

      const [ toA, toB ] = AsTurn ? [ 2, 0 ] : [ 0, 2 ];
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      const outcome = AsTurn ? 'A won' : 'B won';
      commit();

      sendOutcome(outcome)(); } );
