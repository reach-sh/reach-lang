'reach 0.1';

// Protocol
const DELAY = 20; // in blocks
const howMany = 2;
const [ isOutcome, A_TIMEOUT, B_TIMEOUT, A_WON, B_WON ] = makeEnum(4);

const Player =
      { ...hasRandom,
        getMove: Fun([Array(UInt, 2)], Tuple(UInt, UInt)),
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
        .timeout(relativeTime(DELAY), () => closeTo(A, sendOutcome(B_TIMEOUT)));
      commit();

      A.only(() => {
        const coinFlipA = declassify(_coinFlipA); });
      A.publish(coinFlipA)
        .timeout(relativeTime(DELAY), () => {
          closeTo(B, sendOutcome(A_TIMEOUT));
        });
      require(commitA == digest(coinFlipA));
      const AisFirst = (((coinFlipA % 2) + (coinFlipB % 2)) % 2) == 0;

      var [ AsTurn, heaps ] =
        [ AisFirst, Array.iota(howMany).map(_ => initialHeap) ];
      invariant(balance() == (2 * wagerAmount));
      while ( heaps.reduce(0, add) > 0 ) {
        const doMove = (now, next, who_timeout) => {
          now.only(() => {
            const _move = interact.getMove(heaps);
            const [ choice, amount ] = declassify(_move);
            assume(choice < howMany);
            assume(amount <= heaps[choice]); });
          now.publish(choice, amount)
            .timeout(relativeTime(DELAY), () =>
              closeTo(next, sendOutcome(who_timeout)));

          require(choice < howMany);
          require(amount <= heaps[choice]);
          return [ !AsTurn, heaps.set(choice, heaps[choice] - amount) ]; };

        if ( AsTurn ) {
          commit();
          const nextSt = doMove(A, B, A_TIMEOUT);
          [ AsTurn, heaps ] = nextSt;
          continue;
        } else {
          commit();
          const nextSt = doMove(B, A, B_TIMEOUT);
          [ AsTurn, heaps ] = nextSt;
          continue; } }

      const [ toA, toB ] = AsTurn ? [ 2, 0 ] : [ 0, 2 ];
      transfer(toA * wagerAmount).to(A);
      transfer(toB * wagerAmount).to(B);
      const outcome = AsTurn ? A_WON : B_WON;
      commit();

      sendOutcome(outcome)(); } );
