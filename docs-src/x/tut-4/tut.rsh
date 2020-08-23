'reach 0.1';

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);
const winner = (handA, handB) =>
      ((handA + (4 - handB)) % 3);

forall(UInt256, handA =>
  forall(UInt256, handB =>
    assert(isOutcome(winner(handA, handB)))));
assert(winner(ROCK, PAPER) == B_WINS);
assert(winner(PAPER, ROCK) == A_WINS);
assert(winner(ROCK, ROCK) == DRAW);
assert(forall(UInt256, (hand) =>
  winner(hand, hand) == DRAW));

const Player =
      { getHand: Fun([], UInt256),
        seeOutcome: Fun([UInt256], Null) };
const Alice =
      { ...Player,
        wager: UInt256 };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt256], Null) };

export const main =
  Reach.App(
    {},
    [['Alice', Alice], ['Bob', Bob]],
    (A, B) => {
      A.only(() => {
        const wager = declassify(interact.wager);
        const handA = declassify(interact.getHand()); });
      A.publish(wager, handA)
        .pay(wager);
      // XXX unknowable(B, A(handA));
      commit();

      B.only(() => {
        interact.acceptWager(wager);
        const handB = declassify(interact.getHand()); });
      B.publish(handB)
        .pay(wager);

      const outcome = winner(handA, handB);
      const [forA, forB] =
            outcome == B_WINS ? [0, 2] :
            outcome == DRAW ? [1, 1] :
            [2, 0];
      transfer(forA * wager).to(A);
      transfer(forB * wager).to(B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
