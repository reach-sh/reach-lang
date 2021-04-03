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

const batchSize = 5;

const getBatch = (getHand) =>
  Array.iota(batchSize).map((_) => getHand());

const batchWinner = (handsA, handsB) =>
  true ?
  Array.iota(batchSize).map((i) =>
    winner(handsA[i], handsB[i])).reduce(DRAW, (x, y) =>
      x == DRAW ? y : x)
  : handsA.zip(handsB).reduce(DRAW, ((o, [hA, hB]) =>
    o == DRAW ? winner(hA, hB) : o));

const Player =
      { ...hasRandom,
        getHand: Fun([], UInt),
        getBatch: Fun([], Array(UInt, batchSize)),
        seeOutcome: Fun([UInt, Array(UInt, batchSize), Array(UInt, batchSize)], Null),
        informTimeout: Fun([UInt], Null) };
const Alice =
      { ...Player,
        wager: UInt,
        DEADLINE: UInt,
         };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt, UInt], Null) };

export const main =
  Reach.App(
    {},
    [Participant('Alice', Alice), Participant('Bob', Bob)],
    (A, B) => {
      const informTimeout = (who) => {
        each([A, B], () => {
          interact.informTimeout(who); }); };

      A.only(() => {
        const wager = declassify(interact.wager); 
        const DEADLINE = declassify(interact.DEADLINE);
        const _AFirstBatch = interact.getBatch();//interact.firstBatch;
        const [_AFirstBatchCommitment, _AFirstBatchSalt] = makeCommitment(interact, _AFirstBatch);
        const AFirstCommit = declassify(_AFirstBatchCommitment);
      });

      A.publish(wager, DEADLINE, AFirstCommit)
        .pay(wager);
      commit();

      unknowable(B, A(_AFirstBatchSalt, _AFirstBatch))
      B.only(() => {
        interact.acceptWager(wager, DEADLINE); 
        const BFirstBatch = declassify(interact.getBatch());
      });
      B.publish(BFirstBatch)
       .pay(wager)
        .timeout(DEADLINE, () => closeTo(A, () => {informTimeout(1)}));
      commit();

      A.only(() => {
        const [AFirstBatchSalt, AFirstBatch] = declassify([_AFirstBatchSalt, _AFirstBatch]);
      });
      A.publish(AFirstBatchSalt, AFirstBatch)
       .timeout(DEADLINE, () => closeTo(B, () => {informTimeout(0)}));
      checkCommitment(AFirstCommit, AFirstBatchSalt, AFirstBatch);

      var [outcome, round, ABatch, BBatch] = [batchWinner(AFirstBatch, BFirstBatch), 0, Array.iota(batchSize), Array.iota(batchSize)];
      invariant(balance() == 2 * wager && isOutcome(outcome) );
      while ( outcome == DRAW ) {
        const doRound = (First, Second, fWho, sWho) => {
          commit();

          First.only(() => {
            const _BatchFirst = interact.getBatch();
            const [_commitFirst, _saltFirst] = makeCommitment(interact, _BatchFirst);
            const commitFirst = declassify(_commitFirst); });
          First.publish(commitFirst)
            .timeout(DEADLINE, () => closeTo(Second, () => {informTimeout(fWho)}));
          commit();

          unknowable(Second, First(_BatchFirst, _saltFirst));
          Second.only(() => {
            const BatchSecond = declassify(interact.getBatch()); });
          Second.publish(BatchSecond)
            .timeout(DEADLINE, () => closeTo(First, () => {informTimeout(sWho)}));
          commit();

          First.only(() => {
            const [saltFirst, BatchFirst] = declassify([_saltFirst, _BatchFirst]); });
          First.publish(saltFirst, BatchFirst)
            .timeout(DEADLINE, () => closeTo(Second, () => {informTimeout(fWho)}));
          checkCommitment(commitFirst, saltFirst, BatchFirst);

          return [BatchFirst, BatchSecond]
        }
        if (round % 2 == 0) {
          const [first, second] = doRound(B,A,1,0);
          [outcome, round, ABatch, BBatch] = [batchWinner(second, first), round + 1, second, first];
          continue; 
        } else {
          const [first, second] = doRound(A,B,0,1);
          [outcome, round, ABatch, BBatch] = [batchWinner(first, second), round + 1, first, second];
          continue; 
        }
       }

      assert(outcome == A_WINS || outcome == B_WINS);
      transfer(2 * wager).to(outcome == A_WINS ? A : B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome, ABatch, BBatch); });
      exit(); });