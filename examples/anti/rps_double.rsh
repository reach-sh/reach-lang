'reach 0.1 exe';

import '../examples/rps/rsh/rps_shared.rsh';

const A = newParticipant();
const B = newParticipant();
const O = newParticipant();

const DELAY = 10; // in blocks

function whoWinsBestOfThree(winCountA, winCountB, lastOutcome) {
  if (winCountA > winCountB) {
    return A_WINS;
  } else {
    if (winCountB > winCountA) {
      return B_WINS;
    } else {
      // only happens if someone leaves
      return lastOutcome;
    }
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


function main() {
  A.only(() => {
    const wagerAmount = declassify(is(uint256, interact.getWagerAmount()));
    const escrowAmount = declassify(is(uint256, interact.getEscrowAmount()));
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

  // Best of 3
  // var [ winCountA, winCountB, roundCount, anyQuitters, lastOutcome ] = [ 0, 0, 0, false, A_WINS ];
  // invariant(balance() == 2 * wagerAmount + escrowAmount);
  // while ( winCountA < 2 && winCountB < 2 && !anyQuitters ) {
  //   const [ count, outcome ] = do_round_1(A, B, wagerAmount, escrowAmount);
  //   assert(outcome != DRAW);
  //   assert(isOutcome(outcome));

  //   const anyQuitThisRound = outcome == A_QUITS || outcome == B_QUITS;
  //   [ winCountA, winCountB, roundCount, anyQuitters, lastOutcome ] = (outcome == A_WINS || outcome == B_QUITS)
  //     ? [ winCountA + 1, winCountB, roundCount + count, anyQuitThisRound, outcome ]
  //     : [ winCountA, winCountB + 1, roundCount + count, anyQuitThisRound, outcome ];
  //   continue;
  // }

  //  just do three
  var [ i, count, outcome ] = [ 0, 0, DRAW ];
  invariant(balance() == 2 * wagerAmount + escrowAmount && isOutcome(outcome));
  while ( i < 3 ) {
    const a_1 = A;
    const b_1 = B;
    const wagerAmount_1 = wagerAmount;
    const escrowAmount_1 = escrowAmount;

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

    
    [ i, count, outcome ] = [ i + 1, count + count_1, outcome_1 ];
    continue;
  }


  // assert(winCountA != winCountB
  //        || (anyQuitters && (lastOutcome == A_QUITS || lastOutcome == B_QUITS)));
  // const outcome = whoWinsBestOfThree(winCountA, winCountB, lastOutcome);
  // const count = roundCount;

  // assert(outcome != DRAW);
  if ( outcome == A_QUITS ) {
    transfer(balance()).to(B); }
  else if ( outcome == B_QUITS ) {
    transfer(balance()).to(A); }
  else {
    const [getsA, getsB] = (() => {
      if (outcome == A_WINS) {
        return [2 * wagerAmount, 0]; }
      else {
        return [0, 2 * wagerAmount]; } })();
    transfer(escrowAmount + getsA).to(A);
    transfer(getsB).to(B); }
  commit();

  interact.whilecount(count);
  interact.outcome();
  return showOutcome(outcome); }
