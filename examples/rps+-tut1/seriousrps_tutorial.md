[Note: for ease of writing pre scribbl formatting, I'm referencing line numbers in terms of their line number within the code block]

Over the course of this tutorial, we'll make rock paper scissors both more efficient and equitable, by reducing transactions, submitting moves in batches to reduce the likelihood of a round ending in a draw, and switching who goes first after a draw to balance transaction costs.

In this section, we make rock paper scissors less expensive by bundling each player's first hand choice with their first transaction commiting the game's wager, allowing us to compute a game outcome after two transactions from Alice and one transaction from Bob. We'll also allow Alice to set the DEADLINE UInt, and Bob to view the DEADLINE as Bob joins.
```javascript
const Player = {
	...hasRandom,
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
- Line 11 adds the DEADLINE to Alice's interact interface
- Line 14 adds another UInt (the DEADLINE) to the input parameters for the acceptWager interact function

We modify Alice's first transaction to include hand-handling. We'll also declassify and publish Alice's DEADLINE, and assert that Alice's hand is unknowable to Bob as per usual:
```
A.only(() => {
  const wager = declassify(interact.wager); 
  const DEADLINE = declassify(interact.DEADLINE);
  const _AFirstHand = interact.getHand();
  const [_AFirstHandCommitment, _AFirstHandSalt] = makeCommitment(interact, _AFirstHand);
  const AFirstCommit = declassify(_DFirstHandsCommitment);
});

A.publish(wager, DEADLINE, AFirstCommit)
  .pay(wager);
commit();

unknowable(B, A(_AFirstHandsSalt, _AFirstHands))
```
- Lines 4-6 follow the typical structure of accessing a secret value and creating a cryptographic commit within the local step. For review, Line 4 defines the secret value of \_AFirstHand from a call to interact.getHand(), 5 commits it, and 6 declassifies the commitment that will be used to verify the secret once it's declassified later.
- Line 9 publishes the wager along with the new values DEADLINE and AFirstCommit
- Line 13 performs the typical verification that Alice's secrets are unknowable to Bob

Now we add hand-handling and publishing to Bob's first transaction, declassify A's hand, and initialize the loop variable outcome with the result of the first round:
```
unknowable(B, A(_AFirstHandSalt, _AFirstHand))
B.only(() => {
  interact.acceptWager(wager, DEADLINE); 
  const BFirstHand = declassify(interact.getHand());
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
- Line 4 declassifies Bob's firstHand; because Alice's hand has already been committed there's no risk of Alice cheating using Bob's hand.
- Lines 11-15 perform the typical steps of declassifying previously committed secret values, publishing them, and checking them against the crytographic commitment published earlier.
- Line 17 computes the winner of the first round

Our new program requires one less transaction per player to compute a game outcome. To test it, we simply modify our command line interface (CLI) to initialize interact.firstHand to the value of await getHand(), for both Alice and Bob. We also allow Alice to enter a DEADLINE, and Bob to view the DEADLINE.
```
if (isAlice) {
  const amt = await ask(
    `How much do you want to wager?`,
    stdlib.parseCurrency
  );
  interact.wager = amt;
  const deadline = await ask(
    'How many blocks until a timeout?', (x) => x);
  interact.DEADLINE = deadline;
} else {
  interact.acceptWager = async (wager, deadline) => {
    const accepted = await ask(
      `Do you accept the wager of ${fmt(wager)}? with the deadline of ${deadline} blocks`,
      yesno
    );
    if (accepted) {
      return;
    } else {
      process.exit(0);
    }
  };
}
```
- Lines 7-8 ask Alice for a deadline and assign it to interact.DEADLINE
- Line 13 communicates both the wager and deadline to Bob
- Since interact.getHand is already defined, the backend will call getHand to get the participants' first hands at the specified time.

In the next tutorial, we'll make draws, and thereby multiple transactions per player, significantly less likely by collecting hands in a batch and computing the winner based on the first non-draw sub-round.

https://github.com/nicholasburka/rps-gui/blob/vuecli/reach/tut2/seriousrps_tutorial2.md