'reach 0.1';

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

const winner = (handAlice, handBob) =>
      ((handAlice + (4 - handBob)) % 3);

assert(winner(ROCK, PAPER) == B_WINS);
assert(winner(PAPER, ROCK) == A_WINS);
assert(winner(ROCK, ROCK) == DRAW);

forall(UInt, handAlice =>
  forall(UInt, handBob =>
    assert(isOutcome(winner(handAlice, handBob)))));

forall(UInt, (hand) =>
  assert(winner(hand, hand) == DRAW));

const Player =
      { ...hasRandom,
        getHand: Fun([], UInt),
        seeOutcome: Fun([UInt], Null),
        informTimeout: Fun([], Null) };
const Alice =
      { ...Player,
        wager: UInt };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt], Null) };

const DEADLINE = 30;
export const main =
  Reach.App(
    {},
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      const informTimeout = () => {
        each([A, B], () => {
          interact.informTimeout(); }); };

      A.only(() => {
        const wager = declassify(interact.wager); });
      A.publish(wager)
        .pay(wager);
      commit();

      B.only(() => {
        interact.acceptWager(wager); });
      B.pay(wager)
        .timeout(DEADLINE, () => closeTo(A, informTimeout));

      var outcome = DRAW;
      invariant(balance() == 2 * wager && isOutcome(outcome) );
      while ( outcome == DRAW ) {
        commit();

        A.only(() => {
          const _handAlice = interact.getHand();
          const [_commitA, _saltA] = makeCommitment(interact, _handAlice);
          const commitA = declassify(_commitA); });
        A.publish(commitA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        commit();

        unknowable(B, A(_handAlice, _saltA));
        B.only(() => {
          const handBob = declassify(interact.getHand()); });
        B.publish(handBob)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));
        commit();

        A.only(() => {
          const [saltA, handAlice] = declassify([_saltA, _handAlice]); });
        A.publish(saltA, handAlice)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        checkCommitment(commitA, saltA, handAlice);

        outcome = winner(handAlice, handBob);
        continue; }

      assert(outcome == A_WINS || outcome == B_WINS);
      transfer(2 * wager).to(outcome == A_WINS ? A : B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
