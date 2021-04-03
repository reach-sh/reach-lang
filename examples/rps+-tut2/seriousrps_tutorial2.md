In order to make draws less likely, we can collect a batch of moves from each player, and compute an outcome by finding the first hand in a batch that returns a winner (or returning a draw if there is none). First, we specify the batchSize, a getBatch function - either from the interact interface or by repeatedly calling getHand(), and a batchWinner function that computes a winner given two batches. Jay outlines the two approaches to get a batch, and two approaches to computer a batchWinner: 
```
const batchSize = 5;

// This program shows two different ways to get a batch: either have the
// backend call the frontend many times (what Bob does) with this function, or
// add a new function to the frontend that returns many results (what Alice
// does). The advantage of Bob's approach is that the frontend doesn't need to
// know the size of the input, but the downside is that is that it is harder to
// make a coherent interface, because each request for input stands alone.
const getBatch = (getHand) =>
  Array.iota(batchSize).map((_) => getHand());

// Similarly, we have two different approaches for calculating the winner. The
// first uses a loop over the batchSize and then combines the results after,
// while the second uses zip and reduce to do it all at once. I suspect that
// most C-style programmers would try the first thing and most functional-style
// programmers would try the second. Reach supports both styles.
const batchWinner = (handsA, handsB) =>
  true ?
  Array.iota(batchSize).map((i) =>
    winner(handsA[i], handsB[i])).reduce(DRAW, (x, y) =>
      x == DRAW ? y : x)
  : handsA.zip(handsB).reduce(DRAW, ((o, [hA, hB]) =>
    o == DRAW ? winner(hA, hB) : o));

const Player =
      { ...hasRandom,
        firstBatch: Array(UInt, batchSize),
        getHand: Fun([], UInt),
        getBatch: Fun([], Array(UInt, batchSize)),
        seeOutcome: Fun([UInt], Null),
        informTimeout: Fun([], Null) };
```
- Line 28 changes firstHand to firstBatch, using the syntax for specifying an Array in Reach - the type UInt is the first parameter, and the size batchSize is the second parameter. 
- Line 30 includes the getBatch function, which returns the same type of Array
- In Lines 18-22, the winner is calculated 'C-style'. Array.iota(batchSize) creates an array [0,1,2,3,4] that can be used to map the outcome of our regular winner function for each hand in handsA and handsB, which in the C-style syntax is afterwards sequentially reduced by a function selecting the next hand if this hand is a DRAW, or returning this hand if it's not a DRAW (using the conditional expression syntax cond ? true_val : false_val)
- In Lines 21-22, 'functional style' accomplishes the same thing, by combining handsA and handsB into one array [(handA1, handB1), (handA2, handB2)...] where either the next winner is returned if this outcome is DRAW, or the outcome is returned. In both cases, on the last hand in a batch, DRAW is returned.

Now we modify the program so any reference to getHand becomes getBatch, and outcomes are computed by batchWinner. 

```
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
        const AFirstBatch = interact.firstBatch;
        const [_AFirstBatchCommitment, _AFirstBatchSalt] = makeCommitment(interact, _AFirstBatch);
        const AFirstCommit = declassify(_AFirstBatchCommitment);
      });

      A.publish(wager, DEADLINE, AFirstCommit)
        .pay(wager);
      commit();

      unknowable(B, A(_AFirstBatchSalt, _AFirstBatch))
      B.only(() => {
        interact.acceptWager(wager); 
        const BFirstBatch = interact.firstBatch;
      });
      B.publish(BFirstBatch)
       .pay(wager)
        .timeout(DEADLINE, () => closeTo(A, informTimeout));

      A.only(() => {
        const [AFirstBatchSalt, AFirstBatch] = declassify([_AFirstBatchSalt, _AFirstBatch]);
      });
      A.publish(AFirstBatchSalt, AFirstBatch)
       .timeout(DEADLINE, () => closeTo(B, informTimeout));
      checkCommitment(AFirstCommit, AFirstBatchSalt, AFirstBatch);

      var outcome = batchWinner(AFirstBatch, BFirstBatch);
      invariant(balance() == 2 * wager && isOutcome(outcome) );
      while ( outcome == DRAW ) {
        commit();

        A.only(() => {
          const _BatchA = interact.getBatch();
          const [_commitA, _saltA] = makeCommitment(interact, _BatchA);
          const commitA = declassify(_commitA); });
        A.publish(commitA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        commit();

        unknowable(B, A(_BatchA, _saltA));
        B.only(() => {
          const BatchB = declassify(interact.getBatch()); });
        B.publish(BatchB)
          .timeout(DEADLINE, () => closeTo(A, informTimeout));
        commit();

        A.only(() => {
          const [saltA, BatchA] = declassify([_saltA, _BatchA]); });
        A.publish(saltA, BatchA)
          .timeout(DEADLINE, () => closeTo(B, informTimeout));
        checkCommitment(commitA, saltA, BatchA);

        outcome = batchWinner(AFirstBatch, BFirstBatch);
        continue; }

      assert(outcome == A_WINS || outcome == B_WINS);
      transfer(2 * wager).to(outcome == A_WINS ? A : B);
      commit();

      each([A, B], () => {
        interact.seeOutcome(outcome); });
      exit(); });
```
- We change every reference from hand to batch, in this case find-and-replace works
- In Line 35 and 63 the winner function call is changed to batchWinner 

The CLI changes similarly, by setting the batchSize, adding a getBatch function and replacing references to firstHand and getHand with firstBatch and getBatch.
```
const batchSize = 5;
const getBatch = async () => {
  var batch = [];
  var hand = undefined;
  for (var i = 0; i < batchSize; i++) {
    hand = await getHand();
    batch.push(hand);
  }
  console.log("submitting batch " + batch);
  return batch;
}
interact.getBatch = getBatch;

if (isAlice) {
  const amt = await ask(
    `How much do you want to wager?`,
    stdlib.parseCurrency
  );
  interact.wager = amt;
  const deadline = await ask(
    'How many blocks until a timeout?', (x) => x);
  interact.DEADLINE = deadline;
  interact.firstBatch = await getBatch();
} else {
  interact.acceptGame = async (wager, deadline) => {
    const accepted = await ask(
      `Do you accept the wager of ${fmt(wager)}? with the deadline of ${deadline} blocks`,
      yesno
    );
    if (accepted) {
      interact.firstBatch = await getBatch();
      return;
    } else {
      process.exit(0);
    }
  };
}
```
- Line 1 defines the batchSize (we could get this from the contract, but it's not necessary)
- Line 2 - 8 define the getBatch function, which fills an array by calling getHand batchSize times
- Line 11 assigns getBatch to the interact interface
- Line 22 and 30 change firstHand to firstBatch

Now the Reach program is much less likely to return a DRAW at the end of any given round, reducing the likelihood of transactions & thereby associated fees. 

In the next section, we modify our while loop to alternate who goes first, so that the program rotates the extra transaction cost paid by the first person in a round.