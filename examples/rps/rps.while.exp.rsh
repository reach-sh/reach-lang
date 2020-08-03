'reach 0.1';

import "rps.shared.rsh";

// XXX Move into other file when the target is a command line argument

// XXX Also abstract more of this into functions
export const main =
  Reach.App(
    {},
    [["A", Alice], ["B", Bob], ["O", {}]],
    function (A, B, O) {
      A.only(() => {
        const [wagerAmount, escrowAmount] =
              declassify(interact.getParams()); });
      A.publish(wagerAmount, escrowAmount)
        .pay(wagerAmount + escrowAmount);
      commit();

      B.only(() => {
        interact.acceptParams(wagerAmount, escrowAmount); });
      B.pay(wagerAmount)
        .timeout(DELAY, closeTo(A, showOutcome(B_QUITS)));

      var [ count, outcome ] = [ 0, DRAW ];
      invariant((balance() == ((2 * wagerAmount) + escrowAmount))
                && isOutcome(outcome)
                && outcome != A_QUITS
                && outcome != B_QUITS);
      while ( outcome == DRAW ) {
        commit();
        
        A.only(() => {
          const _handA = getHand(interact);
          const [_commitA, _saltA] = makeCommitment(_handA);
          const commitA = declassify(_commitA);
          interact.commits(); });
        A.publish(commitA)
          .timeout(DELAY, closeTo(B, showOutcome(A_QUITS)));
        commit();

        B.only(() => {
          const handB = declassify(getHand(interact));
          interact.shows(); });
        B.publish(handB)
          .timeout(DELAY, closeTo(A, showOutcome(B_QUITS)));
        require(isHand(handB));
        commit();

        A.only(() => {
          const saltA = declassify(_saltA);
          const handA = declassify(_handA);
          interact.reveals(showHand(handB)); });
        A.publish(saltA, handA)
          .timeout(DELAY, closeTo(B, showOutcome(A_QUITS)));
        checkCommitment(commitA, saltA, handA);
        require(isHand(handA));
        const roundOutcome = winner(handA, handB);
        assert(implies(roundOutcome == A_WINS, isHand(handA)));
        assert(implies(roundOutcome == B_WINS, isHand(handB)));
        fair_game(handA, handB, roundOutcome);

        [ count, outcome ] = [ 1 + count, roundOutcome ];
        continue; }

      assert(outcome != DRAW);

      const [getsA, getsB] = (() => {
        if (outcome == A_WINS) {
          return [2 * wagerAmount, 0]; }
        else if (outcome == B_WINS) {
          return [0, 2 * wagerAmount]; }
        else {
          return [wagerAmount, wagerAmount]; } })();
      transfer(escrowAmount + getsA).to(A);
      transfer(getsB).to(B);
      commit();

      return showOutcome(outcome); });
