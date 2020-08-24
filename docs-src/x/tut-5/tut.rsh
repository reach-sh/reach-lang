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

const DELAY = 10;

export const main =
  Reach.App(
    {},
    [['Alice', Alice], ['Bob', Bob]],
    (A, B) => {
      function informTimeout() {
        each([A, B], () => {
          interact.informTimeout(); }); }
      
      A.only(() => {
        const _handA = interact.getHand();
        const [_commitA, _saltA] = makeCommitment(interact, _handA);
        const [wager, commitA] = declassify([interact.wager, _commitA]); });
      A.publish(wager, commitA)
        .pay(wager)
      commit();

      unknowable(B, A(_handA, _saltA));
      B.only(() => {
        interact.acceptWager(wager);
        const handB = declassify(interact.getHand()); });
      B.publish(handB)
        .pay(wager)
        .timeout(DELAY, () => closeTo(A, informTimeout));
      commit();

      A.only(() => {
        const [saltA, handA] = declassify([_saltA, _handA]); });
      A.publish(saltA, handA)
        .timeout(DELAY, () => closeTo(B, informTimeout));
      checkCommitment(commitA, saltA, handA);

      const outcome = winner(handA, handB);
      const [forA, forB] =
            outcome == A_WINS ? [2, 0] :
            outcome == B_WINS ? [0, 2] :
            [1, 1];
      transfer(forA * wager).to(A);
      transfer(forB * wager).to(B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
