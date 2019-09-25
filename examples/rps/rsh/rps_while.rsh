'reach 0.1 exe';

import "rps_shared.rsh";

const A = participant({
  _wagerAmount: uint256,
  _escrowAmount: uint256});

const B = participant({});

const O = participant({});

const DELAY = 10; // in blocks

function main() {
  A.only(() => {
    const wagerAmount = declassify(_wagerAmount);
    const escrowAmount = declassify(_escrowAmount);
    interact.params(); });
  A.publish(wagerAmount, escrowAmount)
    .pay(wagerAmount + escrowAmount)
    .timeout(DELAY, B, () => {
      commit();
      return A_QUITS; });
  commit();

  B.only(() => {
    interact.accepts(wagerAmount, escrowAmount); });
  B.pay(wagerAmount)
    .timeout(DELAY, A, () => {
      transfer(balance()).to(A);
      commit();
      return B_QUITS; });

  var outcome = DRAW;
  invariant((balance() == ((2 * wagerAmount) + escrowAmount))
            && isOutcome(outcome));
  while ( outcome == DRAW ) {
    commit();

    A.only(() => {
      const _handA = getHand();
      const [_commitA, _saltA] = precommit(_handA);
      const commitA = declassify(_commitA);
      interact.commits(); });
    A.publish(commitA)
      .timeout(DELAY, B, () => {
        outcome = A_QUITS;
        continue; });
    commit();

    B.only(() => {
      const handB = declassify(getHand());
      interact.shows(); });
    B.publish(handB)
      .timeout(DELAY, A, () => {
        outcome = B_QUITS;
        continue; });
    require(isHand(handB));
    commit();

    A.only(() => {
      const saltA = declassify(_saltA);
      const handA = declassify(_handA);
      interact.reveals(showHand(handB)); });
    A.publish(saltA, handA)
      .timeout(DELAY, B, () => {
        outcome = A_QUITS;
        continue; });
    check_commit(commitA, saltA, handA);
    require(isHand(handA));
    const this_outcome = winner(handA, handB);
    assert(implies(this_outcome == A_WINS, isHand(handA)));
    assert(implies(this_outcome == B_WINS, isHand(handB)));
    fair_game(handA, handB, this_outcome);

    outcome = this_outcome;
    continue; }

  assert(outcome != DRAW);
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

  interact.outcome();
  return showOutcome(outcome); }
