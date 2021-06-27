#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-battleship")
@title[#:version reach-vers #:tag TAG]{Workshop: Battleship}

In this workshop we'll design an application that allows users to wager against each other in a game of Battleship. This is
a simplified version of Battleship where each player takes one turn to select their ship locations and one more turn to guess
where their opponent has placed their ships.

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
  @item{This program involves 2 parties: The Deployer (Alice) who sets the wager and deploys the contract and the
    Attacher (Bob) who attaches to the contract and accepts the wager.}
  @item{The game starts by Alice placing a wager and deploying the contract. The contract info is returned to Alice to share with Bob.}
  @item{Bob uses the contract provided by Alice to attach to the contract.}
  @item{Bob can choose to accept the wager or timeout and end the game.}
  @item{Alice select where to place her ships.}
  @item{Bob selects where to place his ships.}
  @item{Alice selects where she thinks Bob placed his ships.}
  @item{Bob selects where he thinks Alice placed her ships.}
  @item{The number of correct guesses each has made on the location of thier opponent's ships is counted and either
    a winner is determined or a draw occurs and game starts over at ship selection.}
]

@(drstep-dd TAG)

The data type representation of this program will be very similar to the Rock Paper Scissors tutorial. The only major difference
will be that the interaction interface method @tech{getHand()} will take an input of a Uint Array instead of just a Uint. We will
also need 2 interaction methods, one for taking the user's ship locations and one for taking guesses for their opponent's ship locations.

Take a moment to construct the interaction interface for the participants yourself before looking at my answers.

@(drstep-dd-stop)

@margin-note{It is worth noting that Reach does not support arbitrarily sized arrays, so we
could not determine @tt{NUM_OF_WINNERS} at runtime, e.g. from the interaction interface.
However, we can still write a program that is generic in the size of the array, then
specialize it when we compile.
}

Let's start with defining the player objects. If you've already completed the Rock Paper Scissors tutorial, much of this should be familiar to you already.
First we have the hasRandom interface being added. This is primarily used to allow encryption on the backend as the backend now expects the frontend
to provide for randomness.

The next few, @tech{seeOutcome} and @tech{informTimeout} are both used to notify the players when a certain event occurs. @tech{seeOutcome} is used to return the outcome of the game to both players.
It returns an unsigned integer to the frontend as the outcome of the game. The next interface method, informTimout, is used to notify the player when their opponent has not responded
to the game in time. This is necessary to avoid losing funds if someone drops out from the game. I have added the DEADLINE constant to this portion of the example to point out that
the constant is not used until later in the program.

Now for the more interesting part of the application and the portion that deviated from the tutorial. The @tech{selectShips} and @tech{guessShips} interface methods are very similar to @tech{getHand}
however you may notice that instead of requesting an unsigned integer from the frontend, they are instead requesting an array of unsigned integers. @tech{selectShips} is used to 
return an array from the user's input on where the user would like to place their ships. The @tech{guessShips} methods again takes an array from the user which is then be used to check how many
of their selected locations matches where their opponent selected to place their ships.

Looking at the deployer and the attacher we can see a key difference. The Deployer stores a wager variable within it's object which is inputted when the contract is being deployed.
The Attacher on the other hand has an interface method @tech{acceptWager} which returns the wager amount set by the Deployer.

My @tech{participant interact interface} looks like:

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 11 29 " // ..."]

@(drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication and transfer among the participants.
We should write down this structure as comments in our program to serve as an outline and guide us in implementation.

@(drstep-cc-stop1)

Here's what I wrote for my outline:

@reach{
  // 1. The deployer deploys the contract and sets the wager.
  // 2. The attacher attaches to the contract and accepts the wager.
  // 3. While the outcome of the game is a draw:
  //      3a. Get the user selected ship locations
  //      3b. Get the guesses that a user makes towards the location of their opponent's ships.
  //      3c. Calculate the winner based on the most number of correct guesses.
  // 4. Transfer the wager amount to the winner.
}

Now the outline needs to be converted to a real program.

@(drstep-cc-stop2)

The body of your application should look something like this:

@reach{
const winner = (countA, countB) =>
  countA > countB ? A_WINS : countA < countB ? B_WINS : DRAW;

export const main = Reach.App(
  {},
  [Participant('deployer', deployer), Participant('attacher', attacher)],
  (A, B) => {
    const informTimeout = () => {
      each([A, B], () => {
        interact.informTimeout();
      });
    };

    A.only(() => {
      const wager = declassify(interact.wager);
    });
    A.publish(wager).pay(wager);
    commit();

    B.only(() => {
      interact.acceptWager(wager);
    });

    B.pay(wager).timeout(DEADLINE,
      () => closeTo(A, informTimeout));

    var outcome = DRAW;
    invariant(balance() == 2 * wager && isOutcome(outcome));
    while (outcome == DRAW) {
      commit();

      A.only(() => {
        const _shipsA = interact.selectShips();
        const [_commitA, _saltA] = makeCommitment(interact, _shipsA);
        const commitA = declassify(_commitA);
      });
      A.publish(commitA).timeout(DEADLINE,
        () => closeTo(B, informTimeout));
      commit();

      B.only(() => {
        const _shipsB = interact.selectShips();
        const [_commitB, _saltB] = makeCommitment(interact, _shipsB);
        const commitB = declassify(_commitB);
      });
      B.publish(commitB).timeout(DEADLINE,
        () => closeTo(A, informTimeout));
      commit();

      A.only(() => {
        const guessesA = declassify(interact.guessShips());
      });
      A.publish(guessesA).timeout(DEADLINE,
        () => closeTo(B, informTimeout));
      commit();

      B.only(() => {
        const guessesB = declassify(interact.guessShips());
      });
      B.publish(guessesB).timeout(DEADLINE,
        () => closeTo(A, informTimeout));
      commit();

      A.only(() => {
        const [saltA, shipsA] = declassify([_saltA, _shipsA]);
      });
      A.publish(saltA, shipsA);
      checkCommitment(commitA, saltA, shipsA);
      commit();

      B.only(() => {
        const [saltB, shipsB] = declassify([_saltB, _shipsB]);
      });
      B.publish(saltB, shipsB);
      checkCommitment(commitB, saltB, shipsB);

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

      const countA = countA_0 + countA_1 + countA_2 + countA_3
        + countA_4 + countA_5 + countA_6 + countA_7 + countA_8;
      const countB = countB_0 + countB_1 + countB_2 + countB_3
        + countB_4 + countB_5 + countB_6 + countB_7 + countB_8;

      const outcome_hold = winner(countA, countB);

      each([A, B], () => {
        interact.seeOutcome(outcome_hold);
      });

      outcome = outcome_hold;

      continue;
    }

    transfer(2 * wager).to(outcome == A_WINS ? A : B);
    commit();

    exit();
  }
)
}

The overall functionality of application is fairly similar to how Rock Paper Scissors is implemented. The main difference being that
instead of the @tech{getHand} method, we have @tech{selectShips} and @tech{guessShips}. Both of these methods take an input of an array of unsigned integer
instead of just an unsigned integer. Another thing to note is that we only have to encrypt the selected ship locations since the outcome
of the game can't be altered if both players know where each selected their guesses.

The portion used to calculate the outcome of the game is also a deviation from Rock Paper Scissors since we need to compare 2 arrays. For the
outcome to be determined we need to compare the array of A's ship selections to B's guesses and vice-versa. Looking at the code above, it's clear
that I've opted to manually compare the arrays of all 9 entries. I had originally started with a while loop, iterating over the entries and
comparing the selections to the guesses, however, this proved to be very inefficient and slow since either A or B must publish within the
while loop.

Below is what I had originally created to compary the two arrays:

@reach{
  /* Determine who made the most correct guesses */
  var [ x, countA, countB ] = [ 0, 0, 0 ];
  invariant(balance() == wager * 2);
  while(x < GRID_SIZE) {
    each([A, B], () => {
      interact.loadingResult(x);
    });
    commit();

    // B is always the first to pick up this task. This was originally set to Anybody.publish()
    // but this caused A to Post bad request (400) When A tried to pick up the task.
    B.publish();

    [ x, countA, countB ] = [
      x + 1,
      ieq(shipsB[x], guessesA[x]) ? countA + 1 : countA,
      ieq(shipsA[x], guessesB[x]) ? countB + 1 : countB
    ];

    continue;
  }
}

You may notice that B is always the one publishing. This is due to a race condition that is presented when the publish is set to
@tech{Anybody.publish()}. The result would be a 400 post error on the deployer's end since the attacher was always the first
to reach the publication. The deployer then tries to publish but fails since the attacher is already publishing. This is not a
major issue, but somthing to note if trying to reduce errors on the frontend of the application.

@(drstep-ai TAG)

There are several assertions that are required throughout the applications. Go ahead and see if you can find all of the before
proceeding the where I have placed the assertions.

@(drstep-ai-stop1)

First up is the assertion used to verify that the winner function opererates correctly. 

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 31 36 " // ..."]

Next we have the loop invariant. This is the same loop invariant used in Rock Paper Scissors. Basically we want to make sure
that the @tech{balance()} of the contract is the total amount that the two participants wagered. We also want to verify that
the @tech{outcome} variable continues to be an outcome, meaning it's values are either 0, 1 or 2. This portion has taken a bit
of understanding on my part but what I realized what they I was overcomplicated the defenition of a loop invarient. Simply
defining the work invarient is all that is needed to understand what it is, something that does not vary, stays the same
throughout the exection of the loop.

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 64 65 " // ..."]

The next two insertions are important in ensuring that neither party know the ship locations of their opponent.

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 76 77 " // ..."]

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 87 88 " // ..."]

The assertion here is the @tech{checkCommitment(commitment, salt, x)} method. This method is used to verify that the
commitment made by A and B when encrypting ship locations is the digest of salt and x, the salt being _saltA and _saltB
and x being shipsA and shipsB

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 103 115 " // ..."]

Again this assertion is the same a Rock Paper Scissors, it is used to verify that exiting the loop will result in an
outcome that is either @tech{A_WINS} or @tech{B_WINS}.

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t
         'only 160 161 " // ..."]

@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @tech{interact}. Again this is very similar to Rock Paper Scissors. There is only one
extra interation method to contend with to achieve a working application. Go ahead and try to fill in those interactions before
moving forward.

@(drstep-ii-stop)

@reachex[#:show-lines? #t "workshop-battleship/index.rsh"
         #:link #t]

@(drstep-de TAG)

It's time to test our program. Bouncing off the Rock Paper Scissors example, there are only a few key differences here compared
to RPS. Let's focus on the @tech{selectShips} and the @tech{guessShips} methods. These two methods need to return and array
instead of a Uint. The size of the array array needs to be the same as the defined GRID_SIZE There also needs to be some randomness
involved so that the same participant does not win every time. Go ahead and take the time to implement these methods within
your .mjs file.

@(drstep-de-stop)

Here's the JavaScript @tech{frontend} I wrote:

@reachex[#:show-lines? #t "workshop-battleship/index.mjs"
         #:link #t]

Let's see what it looks like when we run the program:

@verbatim{
Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
  Verifying when NO participants are honest
  Verifying when ONLY "attacher" is honest
  Verifying when ONLY "deployer" is honest
Checked 144 theorems; No failures!
...
Starting Battleship...
Attacher being slow
...
Attacher accpeted the wager of 5
Deployer sets ships...
[
  0, 1, 1, 1, 0,
  0, 0, 1, 1
]
Attacher sets ships...
[
  1, 0, 0, 1, 0,
  1, 0, 0, 0
]
Deployer guesses...
[
  0, 1, 1, 0, 0,
  0, 1, 1, 1
]
Attacher guesses...
[
  0, 0, 1, 1, 1,
  0, 0, 1, 0
]
Deployer saw outcome 0
Attacher saw outcome 0
Balance Before:: Deployer: 10, Attacher: 10
Balance After:: Deployer: 4.9999, Attacher: 14.9999
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

At this point you should have a fully functional Battleship game, congratulations!

There are a few things that you can do at this point to expand the application.
@itemlist[
  @item{Make it so that the attacher the deployer take turns guessing.}
  @item{Change the size of the grid}
  @item{Have ships in various sizes, 1x2, 1x3, 2x3, etc... }
]

If you found this workshop rewarding, please let us know on @(the-community-link)!
