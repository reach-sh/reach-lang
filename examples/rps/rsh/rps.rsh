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
  commit();

  B.only(() => {
    interact.accepts(wagerAmount, escrowAmount); });
  B.pay(wagerAmount)
    .timeout(DELAY, closeTo(A, showOutcome(B_QUITS)));
  commit();

  A.only(() => {
    const _handA = getHand();
    const [_commitA, _saltA] = precommit(_handA);
    const commitA = declassify(_commitA);
    interact.commits(); });
  A.publish(commitA)
    .timeout(DELAY, closeTo(B, showOutcome(A_QUITS)));
  commit();

  B.only(() => {
    const handB = declassify(getHand());
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
  check_commit(commitA, saltA, handA);
  require(isHand(handA));
  const outcome = winner(handA, handB);
  assert(implies(outcome == A_WINS, isHand(handA)));
  assert(implies(outcome == B_WINS, isHand(handB)));
  fair_game(handA, handB, outcome);

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

  interact.outcome();
  return showOutcome(outcome); }
