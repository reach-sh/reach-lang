https://github.com/nicholasburka/rps-gui/blob/vuecli/reach/tut2/seriousrps_tutorial2.md

In this section, we'll make the cost distribution more equal between participants, by modifying our while loop. The first player in any round has two transactions, in order to commit their hand secretly and reveal it after the second player's hand has published. It would be more fair if the first person to play rotated, to distribute these costs. We can't rotate this for the initial deployment transactions, but we can rotate during the DRAW while loop. We can do this using if/else blocks, and rotating who goes first based on whether it's an even or odd round.

```
var [outcome, round] = [batchWinner(AFirstBatch, BFirstBatch), 0];
invariant(balance() == 2 * wager && isOutcome(outcome) );
while ( outcome == DRAW ) {
  if (round % 2 == 0) {
    commit();

    B.only(() => {
      const _BatchB = interact.getBatch();
      const [_commitB, _saltB] = makeCommitment(interact, _BatchB);
      const commitB = declassify(_commitB); });
    B.publish(commitB)
      .timeout(DEADLINE, () => closeTo(A, informTimeout));
    commit();

    unknowable(A, B(_BatchB, _saltB));
    A.only(() => {
      const BatchA = declassify(interact.getBatch()); });
    A.publish(BatchA)
      .timeout(DEADLINE, () => closeTo(B, informTimeout));
    commit();

    B.only(() => {
      const [saltB, BatchB] = declassify([_saltB, _BatchB]); });
    B.publish(saltB, BatchB)
      .timeout(DEADLINE, () => closeTo(A, informTimeout));
    checkCommitment(commitB, saltB, BatchB);

    [outcome, round] = [batchWinner(AFirstBatch, BFirstBatch), round + 1];
    continue; 
  } else {
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

    [outcome, round] = [batchWinner(AFirstBatch, BFirstBatch), round + 1];
    continue; 
  }
 }
```
- Line 1 adds the 'round' loop variable and initializes it at 0
- Line 4 specifies that the following code block should run if the round is an even number. Evenness is determined by calculating the remainder of the rond number when divided by 2, using the modulo % operator.
- At the end of each block, we calculate the outcome and increment the round.

Bob goes first in the loop, because Alice paid an extra transaction already. The code is long, because we simply copy and pasted the logic for each round and changed the participant ordering. It's more concise to define a function doRound within our while loop, in which we pass each participant as input within the if/else blocks:

```
var [outcome, round] = [batchWinner(AFirstBatch, BFirstBatch), 0];
invariant(balance() == 2 * wager && isOutcome(outcome) );
while ( outcome == DRAW ) {
  const doRound = (First, Second) => {
    commit();

    First.only(() => {
      const _BatchFirst = interact.getBatch();
      const [_commitFirst, _saltFirst] = makeCommitment(interact, _BatchFirst);
      const commitFirst = declassify(_commitFirst); });
    First.publish(commitFirst)
      .timeout(DEADLINE, () => closeTo(Second, informTimeout));
    commit();

    unknowable(Second, First(_BatchFirst, _saltFirst));
    Second.only(() => {
      const BatchSecond = declassify(interact.getBatch()); });
    Second.publish(BatchSecond)
      .timeout(DEADLINE, () => closeTo(First, informTimeout));
    commit();

    First.only(() => {
      const [saltFirst, BatchFirst] = declassify([_saltFirst, _BatchFirst]); });
    First.publish(saltFirst, BatchFirst)
      .timeout(DEADLINE, () => closeTo(Second, informTimeout));
    checkCommitment(commitFirst, saltFirst, BatchFirst);

    return [BatchFirst, BatchSecond]
  }
  if (round % 2 == 0) {
    const [first, second] = doRound(B,A,round);
    [outcome, round] = [batchWinner(second, first), round + 1];
    continue; 
  } else {
    const [first, second] = doRound(B,A,round);
    [outcome, round] = [batchWinner(first, second), round + 1];
    continue; 
  }
 }
```
- In Lines 4-27, we define a function doRound which takes a Participant Interface 'First' and Participant Interface 'Second'. Once these participant variables are available to the function, all we need to do is write the logic for a round of RPS. 
- Lines 29-30 & 33-34 take the result of the round and input it in the correct order to batchWinner to compute the outcome.

We don't have to modify our CLI, since our interface function declarations remain the same.

Now we have a rock paper scissors program that is efficient and fair. Hooray! 

If you'd like to try the author of this tutorial's version of "serious" Rock Paper Scissors on a live net, visit https://nicholasburka.github.io/rps-gui/dist/index.html