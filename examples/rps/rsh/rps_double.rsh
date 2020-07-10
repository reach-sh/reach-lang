'reach 0.1 exe';
// Note: WIP. Commented out code at the end doesn't compile yet

import 'rps_shared.rsh';

const A = participant({
  _wagerAmount: uint256,
  _escrowAmount: uint256});

const B = participant({});

const O = participant({});

const DELAY = 10; // in blocks

function whoWinsBestOfThree(winCountA, winCountB, lastOutcome) {
  if (winCountA > winCountB) {
    return A_WINS;
  } else if (winCountB > winCountA) {
    return B_WINS;
  } else if (lastOutcome == B_QUITS) {
    return B_QUITS;
  } else if (lastOutcome == A_QUITS) {
    return A_QUITS;
  } else {
    return A_WINS; // XXX A wins by default? shouldn't happen.
  }
}


function do_round_1(a_1, b_1, wagerAmount_1, escrowAmount_1) {
  var [ count_1, outcome_1 ] = [ 0, DRAW ];
  invariant((balance() == ((2 * wagerAmount_1) + escrowAmount_1))
            && isOutcome(outcome_1));
  while ( outcome_1 == DRAW ) {
    commit();

    a_1.only(() => {
      const _handA_1 = getHand();
      const [_commitA_1, _saltA_1] = makeCommitment(_handA_1);
      const commitA_1 = declassify(_commitA_1);
      interact.commits(); });
    a_1.publish(commitA_1)
      .timeout(DELAY, () => {
        b_1.publish();
        [ count_1, outcome_1 ] = [ count_1, A_QUITS ];
        continue; });
    commit();

    b_1.only(() => {
      const handB_1 = declassify(getHand());
      interact.shows(); });
    b_1.publish(handB_1)
      .timeout(DELAY, () => {
        a_1.publish();
        [ count_1, outcome_1 ] = [ count_1, B_QUITS ];
        continue; });
    require(isHand(handB_1));
    commit();

    a_1.only(() => {
      const saltA_1 = declassify(_saltA_1);
      const handA_1 = declassify(_handA_1);
      interact.reveals(showHand(handB_1)); });
    a_1.publish(saltA_1, handA_1)
      .timeout(DELAY, () => {
        b_1.publish();
        [ count_1, outcome_1 ] = [ count_1, A_QUITS ];
        continue; });
    checkCommitment(commitA_1, saltA_1, handA_1);
    require(isHand(handA_1));
    const this_outcome_1 = winner(handA_1, handB_1);
    assert(implies(this_outcome_1 == A_WINS, isHand(handA_1)));
    assert(implies(this_outcome_1 == B_WINS, isHand(handB_1)));
    fair_game(handA_1, handB_1, this_outcome_1);

    [ count_1, outcome_1 ] = [ 1 + count_1, this_outcome_1 ];
    continue; }

  return [ count_1, outcome_1 ]
}

function bestOfThree(a, b, wagerAmount, escrowAmount, doubleOrNothing) {
  var [ winCountA, winCountB, roundCount, anyQuitters, lastOutcome ] = [ 0, 0, 0, false, A_WINS ];
  invariant(balance() == (doubleOrNothing ? 3 : 2) * wagerAmount + escrowAmount);
  while ( winCountA < 2 && winCountB < 2 && !anyQuitters ) {
    const [ count, outcome ] = do_round_1(a, b, wagerAmount, escrowAmount);
    assert(isOutcome(outcome));
    assert(outcome != DRAW);

    const anyQuitThisRound = outcome == A_QUITS || outcome == B_QUITS;
    [ winCountA, winCountB, roundCount, anyQuitters, lastOutcome ] = (outcome == A_WINS || outcome == B_QUITS)
      ? [ winCountA + 1, winCountB, roundCount + count, anyQuitThisRound, outcome ]
      : [ winCountA, winCountB + 1, roundCount + count, anyQuitThisRound, outcome ];
    continue;
  }

  return [ whoWinsBestOfThree(winCountA, winCountB, lastOutcome), roundCount ];
}

function payOut(a, b, wagerAmount, escrowAmount, outcome, count) {
  const [getsA, getsB] = (() => {
    if (outcome == A_WINS || outcome == B_QUITS) {
      return [2 * wagerAmount, 0]; }
    else {
      return [0, 2 * wagerAmount]; } })();
  transfer(escrowAmount + getsA).to(a);
  transfer(getsB).to(b);
  commit();

  interact.whilecount(count);
  interact.outcome();
  return showOutcome(outcome);
}

function payOutDoubleOrNothing(a, b, aLostFirstRound, wagerAmount, escrowAmount, outcome, count) {
  const [getsA, getsB] = (() => {
    if (outcome == A_WINS || outcome == B_QUITS) {
      if (aLostFirstRound) {
        // A has paid in 2 wagerAmount, so "nothing" is getting that amount back
        return [2 * wagerAmount, wagerAmount ];
      } else {
        return [3 * wagerAmount, 0];
      }
    } else { // B_WINS or A_QUITS
      if (aLostFirstRound) {
        return [0, 3 * wagerAmount];
      } else {
        // B has paid in 2 wagerAmount, so "nothing" is getting that amount back
        return [wagerAmount, 2 * wagerAmount];
      }
    }
  })();
  transfer(escrowAmount + getsA).to(a);
  transfer(getsB).to(b);
  commit();

  interact.whilecount(count);
  interact.outcome();
  return showOutcome(outcome);
}

function main() {
  A.only(() => {
    const wagerAmount = declassify(_wagerAmount);
    const escrowAmount = declassify(_escrowAmount);
    interact.params(); });
  A.publish(wagerAmount, escrowAmount)
    .pay(wagerAmount + escrowAmount);
  commit();

  B.only(() => {
    interact.accepts(wagerAmount, escrowAmount); });
  B.pay(wagerAmount)
    .timeout(DELAY, () => {
      A.publish();
      transfer(balance()).to(A);
      commit();
      return showOutcome(B_QUITS); });

  const [ outcome, count ] = bestOfThree(A, B, wagerAmount, escrowAmount, false);
  assert(outcome != DRAW);

  // XXX Delete this and uncomment the following
  return payOut(A, B, wagerAmount, escrowAmount, outcome, count);

  // TODO: reorganize this code to reduce duplication.
  // if (outcome == A_WINS) {
  //   const aLostFirstRound = false;
  //   B.only(() => {
  //     const doubleOrNothing = declassify(interact.doubleOrNothing());
  //     const doubleOrNothingPayment = declassify(doubleOrNothing ? wagerAmount : 0);
  //   });
  //   B.publish(doubleOrNothing, doubleOrNothingPayment)
  //     .pay(doubleOrNothingPayment)
  //     .timeout(DELAY, () => {
  //       A.publish();
  //       return payOut(A, B, wagerAmount, escrowAmount, outcome, count);
  //     });
  //   if (doubleOrNothing) {
  //     const [ outcome2, count2 ] = bestOfThree(A, B, wagerAmount, escrowAmount, true);
  //     assert(outcome2 != DRAW);
  //     return payOutDoubleOrNothing(A, B, aLostFirstRound, wagerAmount, escrowAmount, outcome2, count + count2);
  //   } else {
  //     return payOut(A, B, wagerAmount, escrowAmount, outcome, count);
  //   }
  // } else if (outcome == B_WINS) {
  //   const aLostFirstRound = true;
  //   A.only(() => {
  //     const doubleOrNothing = declassify(interact.doubleOrNothing());
  //     const doubleOrNothingPayment = declassify(doubleOrNothing ? wagerAmount : 0);
  //   });
  //   A.publish(doubleOrNothing, doubleOrNothingPayment)
  //     .pay(doubleOrNothingPayment)
  //     .timeout(DELAY, () => {
  //       B.publish();
  //       return payOut(A, B, wagerAmount, escrowAmount, outcome, count);
  //     });
  //   if (doubleOrNothing) {
  //     const [ outcome2, count2 ] = bestOfThree(A, B, wagerAmount, escrowAmount, true);
  //     assert(outcome2 != DRAW);
  //     return payOutDoubleOrNothing(A, B, aLostFirstRound, wagerAmount, escrowAmount, outcome2, count + count2);
  //   } else {
  //     return payOut(A, B, wagerAmount, escrowAmount, outcome, count);
  //   }
  // } else {
  //   return payOut(A, B, wagerAmount, escrowAmount, outcome, count);
  // }
}
