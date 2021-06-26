#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-battleship")
@title[#:version reach-vers #:tag TAG]{Workshop: Battleship}

In this workshop, we'll design an application that allows users to wager against each other then play a simplified version of Battleship where each player selects their ship locations and guesses their opponents ship locations all in one turn.

@(workshop-deps)
@(workshop-init TAG)

@(drstep-pr TAG)

The first step in any program design is to perform problem analysis and determine what information is relevant to the problem.
When writing decentralized applications in Reach, this information analysis includes an analysis of the set of @tech{participants} involved in a computation.

In this case, let's ask the questions:
@itemlist[
  @item{Who is involved in this application?}
  @item{What information do they know at the start of the program?}
  @item{What information are they going to discover and use in the program?}
  @item{What funds change ownership during the application and how?}
]

You should write your answers in your Reach program (@tt{index.rsh}) using a comment.
@reachin{/* Remember comments are written like this. */}

@(drstep-pr-stop)

Let's see how your answers compare to our answers:

@itemlist[
  @item{This program involves 2 parties: The Deployer (Alice) who sets the wager and deploys the contract and the Attacher (Bob) who attaches to the contract and accepts the wager.}
  @item{The game starts by Alice placing a wager and deploying the contract. The contract info is returned to Alice to share with Bob.}
  @item{Bob uses the contract provided by Alice to attach to the contract.}
  @item{Bob can choose to accept the wager or timeout and end the game.}
  @item{Alice select where to place her ships.}
  @item{Bob selects where to place his ships.}
  @item{Alice selects where she thinks Bob placed his ships.}
  @item{Bob selects where he thinks Alice placed her ships.}
  @item{The number of correct guesses each has made on the location of thier opponent's ships is counted and either a winner is determined or a draw occurts.}
]

@(drstep-dd TAG)

Skip of familiar with RPS:
Let's start with defining the player objects. If you've already completed the Rock Paper Scissors tutorial, much of this should be familiar already.
Starting with the player object, we have a few methods that again should be familiar to you. If not, let's break it down and see what's going on.
First we have the hasRandom interface being added. This is primarily used to allow encryption on the backend as the backend now expects the frontend
to provide for random.

The next few, seeOutcome and informTimeout are both used to notify the players when a certain event occurs. seeOutcome is used to return the outcome of the game to both players.
It return an unsigned integer to the frontend as the outcome of the game. The next interface method, informTimout, is used to notify the player when their opponent has not responded
to the game on time. This is necessary to avoid losing fund if someone drops out from the game. I have added the DEADLINE constant to this portion of the example to point out that
the constant is not used until later in the program.

Now for the more interesting part of the application and the portion that deviated from the tutorial. The selectShips and guessShips interface methods are very similar to getHand
however you may notice that instead of requesting an unsigned integer from the frontend, they are instead requesting and array of unsigned integers. selectShips is used to 
return an array from user input on where they user would like to place their ships. The guessShips methods again takes an array from the user which is then used to check how many
of their selected locations matches where their opponent selected to place their ships.

Looking at the deployer and the attacher we can see a key difference. The Deployer stores a wager variable within it's object which is inputted when the contract is being deployed.
The Attacher on the other hand has an interface method acceptWager which returns the wager amount set by the Deployer.

@reach{
  const DEADLINE = 10;
  const GRID_SIZE = 9;
  const player = {
    ...hasRandom,
    seeOutcome: Fun([UInt], Null),
    informTimeout: Fun([], Null),
    selectShips: Fun([], Array(UInt, GRID_SIZE)),
    guessShips: Fun([], Array(UInt, GRID_SIZE)),
  };
  const deployer = {
    ...player,
    wager: UInt,
  };
  const attacher = {
    ...player,
    acceptWager: Fun([UInt], Null)
  };
}

@(drstep-cc TAG)

The next part of the application starts the game by taking the wager amount from the deployer which is then declassified, published (So that the Attacher can view the wager), and the wager paid for.
I've also included the informTimeout interface method to show how a method, local to the scope of the Reach contract, can be created.

@reach{
  export const main = Reach.App(
    {},
    [Participant('deployer', deployer), Participant('attacher', attacher)],
    (A, B) => {
      const informTimeout = () => {
        each([A, B], () => {
          interact.informTimeout();
        });
      };
    }

    //...

  )
}

@(drstep-cc-stop1)
prestart
@(drstep-cc-stop2)
@(drstep-ai TAG)
@(drstep-ii TAG)
@(drstep-ii-stop)
@(drstep-de TAG)

@reach{
  // ...
  
  A.only(() => {
    const wager = declassify(interact.wager);
  });
  A.publish(wager).pay(wager);
  commit();

  // B accepts wager given an amount of time to accept
  B.only(() => {
    interact.acceptWager(wager);
  });

  B.pay(wager).timeout(DEADLINE, () => closeTo(A, informTimeout));

  // ...

}

@reach{
  var [ loopCount, outcome ] = [ 0, DRAW ];
  invariant(balance() == 2 * wager && isOutcome(outcome));
  while (outcome == DRAW) {
    commit();

    // ...

  }

  // ...
}

This portion of the contract calls the shipSelection method, the array returned from the method
is encrypted using a salting variable. Notice that _commitA is decassified afterward while _saltA
is not.

@reach{
  // ...After commit() from previous code

  // A selects locations for ships and stores it in contract private.
  A.only(() => {
    const _shipsA = interact.selectShips();
    const [_commitA, _saltA] = makeCommitment(interact, _shipsA);
    const commitA = declassify(_commitA);
  });
  A.publish(commitA).timeout(DEADLINE, () => closeTo(B, informTimeout));
  commit();
  // B should not know the location of A's ships
  unknowable(B, A(_shipsA, _saltA));

  // B selects locations for ships and stores them in contract public
  B.only(() => {
    const _shipsB = interact.selectShips();
    const [_commitB, _saltB] = makeCommitment(interact, _shipsB);
    const commitB = declassify(_commitB);
  });
  B.publish(commitB).timeout(DEADLINE, () => closeTo(A, informTimeout));
  commit();
  // A should not know the location of B's ships
  unknowable(A, B(_shipsB, _saltB));

  // ...
}

Now it's time to pick up the user's guesses, this time there is no need to
encrypt the array, instead we simply store the array in their respective
constants and publish them.

@reach{
  // ...

  // A guesses B's ship locations
  A.only(() => {
    const guessesA = declassify(interact.guessShips());
  });
  A.publish(guessesA).timeout(DEADLINE, () => closeTo(B, informTimeout));
  commit();
  // B guesses A's ship locations
  B.only(() => {
    const guessesB = declassify(interact.guessShips());
  });
  B.publish(guessesB).timeout(DEADLINE, () => closeTo(A, informTimeout));
  commit();

  // ...
}

After the guesses have been received from both parties, we go ahead and decrypt
the selected locations using the _salt we had stored. We publish both the salt and
the location of the ships and use the salt as well as the original commitment to
verify that the commitment is the digest of the salt and the ships the the checkCommitment
method.

@reach{
  // ...

  // A decrypts and stores ships locations on contract public
  A.only(() => {
    const [saltA, shipsA] = declassify([_saltA, _shipsA]);
  });
  A.publish(saltA, shipsA);
  checkCommitment(commitA, saltA, shipsA);
  commit();
  // A decrypts and stores ships locations on contract public
  B.only(() => {
    const [saltB, shipsB] = declassify([_saltB, _shipsB]);
  });
  B.publish(saltB, shipsB);
  checkCommitment(commitB, saltB, shipsB);

  // ...
}

Warning! This section is not included in the final contract. I've added this in here
to point out one of the challenges I had while building the application.

I had originally planned on using a loop to iterate over all four array, comparing the
selection of A to the guesses of B and vice versa. This proved to be extremely inefficient
as each iteration of the loop a commit as well as requiring A or B to call publish.


While loop is to slow due to requiring publish per loop.
I devnet it took about 2-3 minutes to run a loop of 9
There would also be the issue of making a transaction per
per loop, this would cost a fee and require too much input
from the user.

I also had an issue where when sent to Anybody.publish() would result
in a race condition between A and B where the participant that did
not publish logs an error on the front-end.

This is why I replaced the loop with manual checks. this is much
faster in comparison making only one transaction compared to 9

@reach{
  // ...

  var [ x, countA, countB ] = [ 0, 0, 0 ];
  invariant(balance() == wager * 2);
  while(x < GRID_SIZE) {
    each([A, B], () => {
      interact.loadingResult(x);
    });
    commit();
    // B is always the first to pick up this task.
    // This was originally set to Anybody.publish()
    // but this caused A to Post bad request (400)
    // When A tried to pick up the task.
    B.publish();

    [ x, countA, countB ] = [
      x + 1,
      ieq(shipsB[x], guessesA[x]) ? countA + 1 : countA,
      ieq(shipsA[x], guessesB[x]) ? countB + 1 : countB
    ];

    continue;
  }
  const outcome_hold = winner(countA, countB);

  // ...
}

So instead of going the route of iterating over each element with a while loop, I have opted for
manually checking each entry of the array. The count for A is determined by comparing B's ships compared to A's guesses, 
and the count for B is determined by comparing A's ships compared to B's guesses. The results of these comparisons is 
is stored in a constant in order to avoid making any commits

@reach{
  const countA_0 = ieq(shipsB[0], guessesA[0]) ? 1 : 0;
  const countB_0 = ieq(shipsA[0], guessesB[0]) ? 1 : 0;

  const countA_1 = ieq(shipsB[1], guessesA[1]) ? 1 : 0;
  const countB_1 = ieq(shipsA[1], guessesB[1]) ? 1 : 0;

  const countA_2 = ieq(shipsB[2], guessesA[2]) ? 1 : 0;
  const countB_2 = ieq(shipsA[2], guessesB[2]) ? 1 : 0;

  const countA_3 = ieq(shipsB[3], guessesA[3]) ? 1 : 0;
  const countB_3 = ieq(shipsA[3], guessesB[3]) ? 1 : 0;

  const countA_4 = ieq(shipsB[4], guessesA[4]) ? 1 : 0;
  const countB_4 = ieq(shipsA[4], guessesB[4]) ? 1 : 0;

  const countA_5 = ieq(shipsB[5], guessesA[5]) ? 1 : 0;
  const countB_5 = ieq(shipsA[5], guessesB[5]) ? 1 : 0;

  const countA_6 = ieq(shipsB[6], guessesA[6]) ? 1 : 0;
  const countB_6 = ieq(shipsA[6], guessesB[6]) ? 1 : 0;

  const countA_7 = ieq(shipsB[7], guessesA[7]) ? 1 : 0;
  const countB_7 = ieq(shipsA[7], guessesB[7]) ? 1 : 0;

  const countA_8 = ieq(shipsB[8], guessesA[8]) ? 1 : 0;
  const countB_8 = ieq(shipsA[8], guessesB[8]) ? 1 : 0;

  const countA = countA_0 + countA_1 + countA_2 + countA_3 + countA_4 + countA_5 + countA_6 + countA_7 + countA_8;
  const countB = countB_0 + countB_1 + countB_2 + countB_3 + countB_4 + countB_5 + countB_6 + countB_7 + countB_8;

  const outcome_hold = winner(countA, countB);
}

And to finish off the loop, below we have we have the results of the outcome. Notice that the outcome of the game is
returned to the frontend before the loop is completed. This is done so that the frontend will know that draw has occurring.
If a draw does occur the loop will continue from the beginning. The frontend is notified in order to restart the game from
the point of the ship selection process.

@reach{
  var [ outcome ] = [ DRAW ];
  invariant(balance() == 2 * wager && isOutcome(outcome));
  while (outcome == DRAW) {
    commit();

    // ...

    const outcome_hold = winner(countA, countB);

    each([A, B], () => {
      interact.seeOutcome(outcome_hold);
    });

    [ outcome ] = [ winner(countA, countB) ];

    continue;
  }

  // ...
}

If a draw does not occur then the wile loop exits. After exiting there is assertion 

@reach{
  // ...

  assert(outcome == A_WINS || outcome == B_WINS);
  transfer(2 * wager).to(outcome == A_WINS ? A : B);
  commit();

  exit();
}

