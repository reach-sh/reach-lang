'reach 0.1';

const [ isHand, ROCK, PAPER, SCISSORS ] = makeEnum(3);
const [ isOutcome, B_WINS, DRAW, A_WINS ] = makeEnum(3);

const winner = (handA, handB) =>
      ((handA + (4 - handB)) % 3);

assert(winner(ROCK, PAPER) == B_WINS);
assert(winner(PAPER, ROCK) == A_WINS);
assert(winner(ROCK, ROCK) == DRAW);

forall(UInt, handA =>
  forall(UInt, handB =>
    assert(isOutcome(winner(handA, handB)))));

forall(UInt, (hand) =>
  assert(winner(hand, hand) == DRAW));

const Player =
      { ...hasRandom,
        firstHand: UInt,
        getHand: Fun([], UInt),
        seeOutcome: Fun([UInt], Null),
        informTimeout: Fun([], Null) };
const Alice =
      { ...Player,
        wager: UInt,
        DEADLINE: UInt,
         };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt], Null) };

//const DEADLINE = 150;
export const main =
  Reach.App(
    {},
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      const informTimeout = () => {
        each([A, B], () => {
          interact.informTimeout(); }); };

      A.only(() => {
        const wager = declassify(interact.wager); 
        const DEADLINE = declassify(interact.DEADLINE);
        const _AFirstHand = interact.firstHand;
        const [_AFirstHandCommitment, _AFirstHandSalt] = makeCommitment(interact, _AFirstHand);
        const AFirstCommit = declassify(_AFirstHandCommitment);
      });

      A.publish(wager, DEADLINE, AFirstCommit)
        .pay(wager);
      commit();

      unknowable(B, A(_AFirstHandSalt, _AFirstHand))
      B.only(() => {
        interact.acceptWager(wager, DEADLINE); 
        const BFirstHand = declassify(interact.firstHand);
      });
      B.publish(BFirstHand)
        .pay(wager)
        .timeout(DEADLINE, () => closeTo(A, informTimeout));
      commit();

      A.only(() => {
        const [AFirstHandSalt, AFirstHand] = declassify([_AFirstHandSalt, _AFirstHand]);
      });
      A.publish(AFirstHandSalt, AFirstHand)
       .timeout(DEADLINE, () => closeTo(B, informTimeout));
      checkCommitment(AFirstCommit, AFirstHandSalt, AFirstHand);

      var outcome = winner(AFirstHand, BFirstHand);
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