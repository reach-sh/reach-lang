In the Reach tutorial, a basic RPS program is developed. In this workshop, we take RPS to the next level by generalizing Rock Paper Scissors to be as cheap and equitable as possible in transaction costs for each player.

Problem Analysis

Let's start by analyzing the structure of transactions in the basic RPS program:
- what are the transactions in a game of rock paper scissors?
- are there any conditions that increase the number of transactions?
- are the transaction costs distributed evenly between participants?
- does the distribution of transaction costs between participants vary, given different logical paths through the game program?


Stop & write your answers.

- The transactions in the basic RPS program are 
- 1. Alice's creation of the game and payment of the wager
- 2. Bob's acceptance of the game wager & payment 
- 3. Alice plays a hand 
- 4. Bob plays a hand, and finally 
- 5. Alice publishes their hand

- If the players draw, then steps 3-5 repeat, adding an additional 3 transactions per round

- Alice is responsible for 3/5 of the transactions, and if there's a draw, Alice is responsible for 2/3 of each additonal round's transactions

The first two questions reflect different potential optimizations on the cost structure of our dApp. The second two questions reflect the structure of cost distribution.

As dApp programmers, we can minimize transactions, by both eliminating unnecessary transactions and optimizing our logic to make less likely any branch of our program that requires further transactions. By removing transactions, users experience the dual benefit of having both cheaper costs and quicker outcomes.

Cost distribution analysis, like memory or runtime analysis, can be analyzed in a program piece by piece to understand its programmatic structure. Jay elaborates the numerical analysis: 

"Transaction costs on consensus networks can be understood as the constants hidden by asymptotic notations when determining the expense of an algorithm when run on a decentralized application. In typical programming contexts, an algorithm that uses 3 log_2 n + 4 n operations is consider equivalent to an algorithm that uses 5 log_4 n + 22 n operations, because constants and bases are ignored in asymptoptic analysis. However, imagine that a program used n local computations and m consensus computations. We’ll call n the "computations" and m the "communications". In this case, the computations are free from the perspective of the consensus network, because they don’t cost network tokens, while the communication cost their price in gas, plus the fee to run them. Therefore, it is often economically efficient to increase n so that m can be smaller.

For example, in the context of tutorial’s version of Rock, Paper, Scissors!, the application uses 2 + 3r communications for a game with r rounds. This is because it takes two communications to set up the loop, then each round of the loop takes three communications. We could make a more complicated version of the application that is optimized in two ways.

First, we could optimize for the common case of when there is no draw and bundle a hand into the opening messages, and use 3 + 3(r - 1) = 3r communications for r rounds, for a saving of two communications. This would slightly increase the complexity of our program by duplicating the submission of hands, but we could easily abstract this into a Reach function.

Second, we could bundle k hands into each communication, so that the number of communications is 3(r//k) for r rounds for a reduction of communications by k times. This is possible through Reach’s ability to deal with array values. The exact value of k would be chosen empirically based on the relative difference in cost between increase message sizes and computations versus the fixed cost of having any transaction at all on the consensus network. Reach’s ability to abstract away the details of communication patterns also us to write this program abstractly and only specify the value of k as a compile-time parameter.

This is a general strategy that is regularly employed in efficient decentralized applications: although a textbook algorithm might say to use a setup phase and many round trips as you divide a space in half each time, it might be vastly more efficient on an actual network to apply meaning-preserving transformations like merging the setup into the loop and dividing the space by much larger constant, like one hundred."

Let's focus on cost optimization before we move onto cost equity.

Cost Optimization Analysis
- Can any transactions can be combined into a single transaction/consensus step?
- How can we design a round of rock paper scissors to make a draw less likely?

Stop & write your answers.

- The first cost optimization we make through submitting the firstHand of each player as they join the game. By simultaneously paying the wager and submitting a hand, one transaction is eliminated for each player.
- One could force a player to randomly choose a different hand if they draw, but this isn't fun or really a proper game of rock paper scissors. Instead, we can ask each player to submit a batch of hands. Then, we can prioritize picking a winner in as few hands as possible from the batch of hands, by deciding our outcome from the first pairing of Alice's hands[x] against Bob's hands[x] that doesn't create a draw. This minimizes the likelihood of a draw, as there's a 6/9 chance in each hands deciding a winner, whereas in a "best-of-x" schema there are additional possibilities for the round to end in an overall draw (using naive probabilistic models that don't factor in that rocks are the primeval basis of human tooling, thus affectively compelling a >1/3 probability across the human population and significantly complicating our models. On average, most humans will choose rocks.).

Data definitions:
- How and where will we have to modify the program to allow batch submissions? 
- How will the participant interfaces change?
- How will the reach program logic change? Will any other functions need modification?

Stop and write your answers.
- The participant interfaces will need a function getHands, returning a UInt array of size x
- We will need a function batchWinner that determines an outcome from the batches submitted by each player.

Try rewriting the program using these optimizations.

Now our program both uses fewer transactions and is less likely to have additional transactions. But it's not quite fair. Alice pays significantly more, to begin the program and if the game results in a draw. Alice proportionally pays more upfront and down the road than Bob.

 - Can any transactions be switched to more evenly distribute costs upfront?
 - Can transactions be switched to more evenly distribute costs down the road?
 - Where else can we consider equalizing costs?

 Write your answers.

 - Since one player's hands have to be hidden while the other player submits, there will necessarily be a 2/1 ratio for each round. So, Alice will have to pay an additional transaction fee upfront. We could balance this by asking an additional half gas fee from Bob that we transfer to Alice.
 - More straightforwardly, we can have Bob pay 2/3 of the second round transactions, and alternate the extra transaction every subsequent round. If we wanted to be extra funky, we could make Bob pay for both the 2nd and 3rd rounds, since a draw is already unlikely and is increasingly unlikely as the game continues.

 Data definitions:
 - How can you alternate who pays the extra transaction each round?

Stop and write your answer.

- An if/else block will get us there. We'll test if it's an even or odd round and use that condition to determine who pays first. Note that as an extra elegancy, instead of simply copy pasting a big code block inside each branch of if and else and changing which participant goes first, we can write a doRound(First, Second) function that takes Participant inputs in the order their transactions should be called. 

Try to write code that alternates the extra transaction each round in a draw, beginning with Bob.

Assertions
What assertions are needed in the your new code? Stop and write your answer.

- Like in our initial round of gameplay, we assert the unknowability of the first hands submitted per round to the second player.
- We also keep the assertion that, before the final transfer, the outcome must be that either player 1 or player 2 wins.

Interaction Introduction
Are there any pieces of contract state that would be useful to communicate to the front-ends that aren't already? (To some extent, this is a matter of personal preference and design).


- I added a UInt Who to informTimeout, to communicate who timed out, and I provided the hands from the winning round to seeOutcome (requiring additional loop variables to store these hands).

My program:
```
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
```

Deployment decision

Now we have a fairer, cheaper Rock Paper Scissors! Congrats. To test, we simply modify index.mjs to get modified user input (an array of hands instead of a single hand) and communicate our new parameters.

```
import { loadStdlib } from '@reach-sh/stdlib';
import * as stdlib from '@reach-sh/stdlib/ALGO.mjs'
import * as backend from './build/index.main.mjs';
import { ask, yesno, done } from '@reach-sh/stdlib/ask.mjs';

(async () => {
  const stdlib = await loadStdlib();

  const isDeployer = await ask(
    `Are you the deployer?`,
    yesno
  );
  const who = isDeployer ? 'Deployer' : 'Attacher';

  console.log(`Starting Rock, Paper, Scissors! as ${who}`);

  let acc = null;
  const createAcc = await ask(
    `Would you like to create an account? (only possible on devnet)`,
    yesno
  );
  if (createAcc) {
    acc = await stdlib.newTestAccount(stdlib.parseCurrency(1000));
  } else {
    const secret = await ask(
      `What is your account secret?`,
      (x => x)
    );
    acc = await stdlib.newAccountFromSecret(secret);
  }

  let ctc = null;
  if (isDeployer) {
    console.log("deploying contract...");
    ctc = acc.deploy(backend);
    const info = await ctc.getInfo();
    console.log(`The contract is deployed as = ${JSON.stringify(info)}`);
  } else {
    const info = await ask(
      `Please paste the contract information:`,
      JSON.parse
    );
    ctc = acc.attach(backend, info);
  }

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async () => fmt(await stdlib.balanceOf(acc));

  const before = await getBalance();
  console.log(`Your balance is ${before}`);

  const interact = { ...stdlib.hasRandom };

  interact.informTimeout = (part_num) => {
    var player_timed_out = undefined;
    if (part_num === 0) {
      player_timed_out = "deployer";
    } else {
      player_timed_out = "attacher";
    }
    console.log(`timeout, ${player_timed_out} was too slow`);
  };

  interact.informDraw = () => {
    console.log(`Draw, play again`);
  };

  interact.informOpponent = (opp) => {
    console.log(opp + " joined your game!");
  };

  const HAND = ['Rock', 'Paper', 'Scissors'];
  const getHand = async () => {
    const HANDS = {
      'Rock': 0, 'R': 0, 'r': 0,
      'Paper': 1, 'P': 1, 'p': 1,
      'Scissors': 2, 'S': 2, 's': 2,
    };

    const hand = await ask(`What hand will you play?`, (x) => {
      const hand = HANDS[x];
      if ( hand == null ) {
        throw Error(`Not a valid hand ${hand}`);
      }
      return hand;
    });
    console.log(`You played ${HAND[hand]}`);
    return hand;
  };

  const getBatch = async () => {
    var hands = [];
    var hand = undefined;
    for (var i = 0; i < 5; i++) {
      hand = await getHand();
      console.log(hand);
      hands.push(hand);
    }
    console.log("hands being submitted");
    console.log(hands);
    //console.log(typeof(hands));
    //console.log(typeof([1,0,0]));
    return hands;
  }

  interact.getBatch = getBatch;

  if (isDeployer) {
    const amt = await ask(
      `How much do you want to wager?`,
      stdlib.parseCurrency
    );
    interact.wager = amt;
    const deadline = await ask(
      'How many blocks until a timeout?', (x) => x);
    interact.deadline = deadline;
  } else {
    interact.acceptGame = async (wager, deadline) => {
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


  const OUTCOME = ['Attacher wins', 'Draw', 'Deployer wins'];
  interact.seeOutcome = async (outcome, dHands, aHands) => {
    console.log(`The outcome is: ${OUTCOME[outcome]}`);
    const p1_move_strs = dHands.map(x => HAND[x]);
    const p2_move_strs = aHands.map(x => HAND[x]);
    console.log('p1 moves: ');
    console.log(p1_move_strs);
    console.log('p2 moves: ');
    console.log(p2_move_strs);
  };

  const part = isDeployer ? backend.Deployer : backend.Attacher;
  await part(ctc, interact);

  const after = await getBalance();
  console.log(`Your balance is now ${after}`);

  done();
})();
```

If you'd like to try the author of this workshop's version of "serious" Rock Paper Scissors on a live net, visit https://nicholasburka.github.io/rps-gui/dist/index.html