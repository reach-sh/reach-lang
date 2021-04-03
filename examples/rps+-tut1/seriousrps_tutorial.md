[For ease of writing pre-scribbl formatting, I'm referencing line numbers in terms of their line number within the code block]

Over the course of this tutorial, we'll make rock paper scissors both more efficient and equitable, by reducing transactions, submitting moves in batches to reduce the likelihood of a round ending in a draw, and switching who goes first after a draw to balance transaction costs.

In this section, we make rock paper scissors less expensive by bundling each player's first hand choice in the same consensus step with their first transaction paying the game's wager, allowing us to compute a game outcome after two transactions from Alice and one transaction from Bob. This requires the extension of the Player interface to include a firstHand field. We'll also allow Alice to set the DEADLINE UInt, and Bob to view the DEADLINE as Bob joins.
```javascript
const Player = {
	...hasRandom,
	firstHand: UInt,
	getHand: Fun([], UInt),
	seeOutcome: Fun([UInt], Null),
	informTimeout: Fun([UInt], Null),
	informDraw: Fun([], Null)
};
const Alice =
      { ...Player,
        wager: UInt,
        DEADLINE: UInt };
const Bob =
      { ...Player,
        acceptWager: Fun([UInt, UInt], Null) };
```
- Line 3 adds the value firstHand to the Player interact interface, with type UInt
- Line 11 adds the DEADLINE to Alice's interact interface
- Line 14 adds another UInt (the DEADLINE) to the input parameters for the acceptWager interact function

Now that firstHand is in the Player interface, we can modify Alice's first transaction to include hand-handling. We'll also declassify and publish Alice's DEADLINE, and assert that Alice's hand is unknowable to Bob as per usual:
```javascript
A.only(() => {
  const wager = declassify(interact.wager); 
  const DEADLINE = declassify(interact.DEADLINE);
  const _AFirstHand = interact.firstHand;
  const [_AFirstHandCommitment, _AFirstHandSalt] = makeCommitment(interact, _AFirstHand);
  const AFirstCommit = declassify(_DFirstHandsCommitment);
});

A.publish(wager, DEADLINE, AFirstCommit)
  .pay(wager);
commit();

unknowable(B, A(_AFirstHandsSalt, _AFirstHands))
```
- Lines 4-6 follow the typical structure of accessing a secret value and creating a cryptographic commit within the local step. For review, Line 4 defines the secret value of \_AFirstHand from interact.firstHand, 5 commits it, and 6 declassifies the commitment that will be used to verify the secret once it's declassified later.
- Line 9 publishes the wager along with the new values DEADLINE and AFirstCommit
- Line 13 performs the typical verification that Alice's secrets are unknowable to Bob

Now we add hand-handling and publishing to Bob's first transaction, declassify A's hand, and initialize the loop variable outcome with the result of the first round:
```javascript
unknowable(B, A(_AFirstHandSalt, _AFirstHand))
B.only(() => {
  interact.acceptWager(wager, DEADLINE); 
  const BFirstHand = declassify(interact.firstHand);
});
B.publish(BFirstHand)
  pay(wager)
  .timeout(DEADLINE, () => closeTo(A, informTimeout));

A.only(() => {
const [AFirstHandSalt, AFirstHand] = declassify([_AFirstHandSalt, _AFirstHand]);
});
A.publish(AFirstHandSalt, AFirstHand)
.timeout(DEADLINE, () => closeTo(B, informTimeout));
checkCommitment(AFirstCommit, AFirstHandSalt, AFirstHand);

var outcome = winner(AFirstHand, BFirstHand);
```
- Line 4 declassifies Bob's firstHand; as in regular RPS the commit/declassify/declassify-checkCommitment publish sequence holds - because Alice's hand has already been committed, there's no risk of Alice cheating using Bob's hand
- Lines 11-15 perform the typical steps of declassifying previously committed secret values, publishing them, and checking them against the crytographic commitment published earlier
- Line 17 computes the winner of the first round

And now our program requires one less transaction per player to compute a game outcome. To test it, we simply modify our command line interface (CLI) to initialize interact.firstHand to the value of await getHand(), for both Alice and Bob. We also allow Alice to enter a DEADLINE, and Bob to view the DEADLINE.
```javascript
if (isAlice) {
  const amt = await ask(
    `How much do you want to wager?`,
    stdlib.parseCurrency
  );
  interact.wager = amt;
  const deadline = await ask(
    'How many blocks until a timeout?', (x) => x);
  interact.DEADLINE = deadline;
  interact.firstHand = await getHand();
} else {
  interact.acceptGame = async (wager, deadline) => {
    const accepted = await ask(
      `Do you accept the wager of ${fmt(wager)}? with the deadline of ${deadline} blocks`,
      yesno
    );
    if (accepted) {
      interact.firstHand = await getHand();
      return;
    } else {
      process.exit(0);
    }
  };

```
- Lines 7-8 ask Alice for a deadline and assign it to interact.DEADLINE
- Line 13 communicates both the wager and deadline to Bob
- Lines 10 & 17 get the firstHand and assign it to Alice & Bob's interact interfaces

In the next tutorial, we'll make draws, and thereby multiple transactions per player, significantly less likely by collecting hands in a batch and computing the winner based on the first non-draw sub-round.