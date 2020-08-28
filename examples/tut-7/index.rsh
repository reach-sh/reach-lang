'reach 0.1';

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

const winner = (handA, handB) =>
      ((handA + (4 - handB)) % 3);

assert(winner(ROCK, PAPER) == B_WINS);
assert(winner(PAPER, ROCK) == A_WINS);
assert(winner(ROCK, ROCK) == DRAW);

forall(UInt256, handA =>
  forall(UInt256, handB =>
    assert(isOutcome(winner(handA, handB)))));

forall(UInt256, (hand) =>
  assert(winner(hand, hand) == DRAW));

const Player =
      { ...hasRandom,
        getHand: Fun([], UInt256),
        seeOutcome: Fun([UInt256], Null),
        informTimeout: Fun([], Null) };
const Alice =
      { ...Player,
        wager: UInt256 };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt256], Null) };

const DEADLINE = 10;
export const main =
  Reach.App(
    {},
    [['Alice', Alice], ['Bob', Bob]],
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
          const _handA = interact.getHand();
          const [_commitA, _saltA] = makeCommitment(interact, _handA);
          const commitA = declassify(_commitA); });
        A.publish(commitA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        commit();

        unknowable(B, A(_handA, _saltA));
        B.only(() => {
          const handB = declassify(interact.getHand()); });
        B.publish(handB)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));
        commit();

        A.only(() => {
          const [saltA, handA] = declassify([_saltA, _handA]); });
        A.publish(saltA, handA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        checkCommitment(commitA, saltA, handA);

        outcome = winner(handA, handB);
        continue; }

      assert(outcome == A_WINS || outcome == B_WINS);
      transfer(2 * wager).to(outcome == A_WINS ? A : B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
