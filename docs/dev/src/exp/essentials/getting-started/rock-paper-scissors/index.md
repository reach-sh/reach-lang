---
menuItem: mi-docs
---

# Rock Paper Scissors

This tutorial shows you how to build Rock Paper Scissors, a command-line and web-based decentralized application that enables two players, Alice and Bob, to compete against each other and wager on the outcome.

> # Install & Setup
> Be sure to complete [Install and Run](/en/books/essentials/getting-started/install-and-run/) and [Set up an IDE](/en/books/essentials/getting-started/set-up-an-ide/) before continuing.

## File Structure
This module uses the following project directory structure:

``` nonum
~/reach/rock-paper-scissors/rps-1-setup
~/reach/rock-paper-scissors/rps-2-rps
~/reach/rock-paper-scissors/rps-3-bets
~/reach/rock-paper-scissors/rps-4-attack
~/reach/rock-paper-scissors/rps-5-trust
~/reach/rock-paper-scissors/rps-6-timeouts
~/reach/rock-paper-scissors/rps-7-loops
~/reach/rock-paper-scissors/rps-8-interact
~/reach/rock-paper-scissors/rps-9-web
```

Begin by creating and navigating to a subdirectory inside your ~/reach/ directory. 

``` bash
$ mkdir -p ~/reach/rock-paper-scissors && cd ~/reach/rock-paper-scissors
```

# Scaffolding and Setup

To make the most of this tutorial, follow along by copying each part of the program and see how things work for yourself. If you’re like us, you may find it beneficial to type each line out, rather than copying & pasting so you can start building your muscle memory and begin to gain a sense for each part of a Reach program.

Create a subdirectory inside the rock-paper-scissors folder for this section of the tutorial. We'll also create a file named `index.rsh`.

``` bash
$ mkdir -p /rps-1-setup/index.rsh
```

> We will be creating subdirectories for each section of this tutorial within the `/rock-paper-scissors/` directory.

Type the following into `index.rsh`:

[rps-1-setup/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-1-setup/index.rsh)

``` js
load: /examples/rps-1-setup/index.rsh
```

> # Links to Source
> You may have noticed that `rps-1-setup/index.rsh` is a link above the code snippet. Click these links to view the full source code.

* Line 1: Indicates that this is a Reach program. This will always be at the top of every index.rsh file.
* Line 3: Defines the main export from the program. The compiler looks at this during run time.
* Lines 4-9: Specify Alice and Bob and the two participants in this application.
* Line 10: Marks the deployment of the Reach program. This enables the program to do things.

Next, we'll create a shell for the JavaScript [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29) code. Create a new file `index.mjs` and add the following:

``` bash
$ touch index.mjs
```

[rps-1-setup/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-1-setup/index.mjs)

``` js
load: /examples/rps-1-setup/index.mjs
```

* Line 1: Imports the Reach standard library loader.
* Line 2: Imports the backend, which will be produced by `$ reach compile`.
* Line 3: Loads the standard library dynamically based on the [REACH_CONNECTOR_MODE](https://docs.reach.sh/ref-usage.html#%28env._.R.E.A.C.H_.C.O.N.N.E.C.T.O.R_.M.O.D.E%29_) environment variable.
* Line 5: Defines the asynchronous function that will contain the body of the frontend.
* Line 6: Defines a quantity of [network tokens](https://docs.reach.sh/ref-model.html#%28tech._network._token%29) as the starting balance for each test account.
* Lines 7 & 8: Create test accounts with endowments for Alice and Bob. This method only works on Reach-provided developer test networks.
* Line 10: Alice deploys the application. This activates the code in `index.rsh`.
* Line 11: Bob attaches to the application.
* Line 13: Awaits for the backend to complete before running the block of code.
* Lines 14 - 16: Initializes a backend for Alice
* Lines 17 - 19: Initializes a backend for Bob
* Line 21: Calls the asynchronous function defined in line 5.

At this point, we can compile and run the program. Be sure Docker is running or the command will not work.

``` bash
$ reach run
```

Assuming no errors, Reach will now build and launch a Docker container for the application. However, our application doesn't do anything yet. At this point in time, you'll only see diagnostic messages.

In the future, you can automate the entire process that we've walked through in this section by running `reach init`. We won't run this now, but in the future, you can use `reach init` to create a boilerplate for your dApps.

In the next step, we'll implement the logic of _Rock, Paper, Scissors!_ so our application begins to do something!

> # Check your Understanding
> When you write a dApp using Reach, do you:
A) Write a smart contract in Solidity, a backend in JavaScript using the Ethereum SDK, and a frontend in JavaScript, then use Reach to test and deploy it.
B) Write a program in Reach that generates a smart contract and a backend and a frontend in JavaScript, then use Reach to test and deploy it?

# Rock, Paper, Scissors

In this section, we'll write enough code such that Alice and Bob will execute a game of _Rock, Paper, Scissors_!

We'll represent the hands of `Rock`, `Paper`, and `Scissors` as the numbers, 0, 1, and 2, respectively.

We'll also represent the three possible outcomes of the game as `B wins`, `A wins`, and `Draw`.

Let's change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move of each player, and later be informed of the game's outcome.

> # Unsigned integers in Reach
> Reach does not support unsigned integers of exactly two bits. Rather than using the integers, 0, 1, and 2, we represent the equivalence class of integers modulo three.

[rps-2-rps/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.rsh)

``` js
load: /examples/rps-2-rps/index.rsh
range: 1-17
```

* Lines 3 - 6: Defines a [participant interact interface](https://docs.reach.sh/ref-programs-appinit.html#%28tech._participant._interact._interface%29) that is shared between the two players. We've defined two methods: `getHand`, which returns a number, and `seeOutcome`, which receives a number.
* Lines 9-14: Establishes the use of the interface for both participants. Now, `interact` will be bound to an object with methods corresponding to the actions defined in the participant interact interface. This connects to the frontend of the corresponding participant.

Let's leave the Reach application and move over to the JavaScript interface to implement these methods in the frontend.

[rps-2-rps/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.mjs)

``` js
load: /examples/rps-2-rps/index.mjs
range: 13-33
```

* Lines 13 & 14: Define arrays to hold the meaning of the hands and outcomes.
* Line 15: Defines a constructor for the Player implementation.
* Lines 16 - 20: Implement the `getHand` method.
* Lines 21 - 23: Implement the `seeOutcome` method.
* Lines 28 * 31: Instantiate the implementation once for Alice and once for Bob. These objects will be bound to [`interact`](https://docs.reach.sh/ref-programs-local.html#%28reach._%28%28interact%29%29%29) in the Reach program.

## Business Logic

There should be nothing interesting or controversial about these implementations; that's the point of Reach. We write normal business logic without worrying about the details of the [consensus network](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) and decentralized application. 

Now we'll return to the Reach program to look inside the body of the program to inspect what actions Alice and Bob are able to take.

In real life, _Rock, Paper, Scissors!_, Alice and Bob simultaneously decide what hand they will play and reveal it at the same time. In practice, "simultaneity" is a complex concept that is difficult to realize. For example, if you've played against a child, you may notice them trying to see what you're going to choose and delay until the last second to reveal their winning hand. Similarly, in a decentralized app, it is not possible to have simultaneity. Instead, one participant must "go first." In this case, we'll select Alice.

The game proceeds in three steps:

1. Alice's backend interacts with its frontend,
1. Gets Alice's hand,
1. And publishes it.

[rps-2-rps/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.rsh)

``` js
load: /examples/rps-2-rps/index.rsh
range: 17-21
```

* Line 17: States that this block of code is something that _only_ Alice performs.
* Line 18: The variable `handAlice` is known only to Alice. This binds its value to the result of interacting with Alice through the `getHand` method found in the JavaScript frontend. `interact` also [declassifies](https://docs.reach.sh/ref-programs-local.html#%28tech._declassify%29) the value, because all information from [frontends](https://docs.reach.sh/ref-model.html#%28tech._frontend%29) is [secret](https://docs.reach.sh/ref-programs-valid.html#%28tech._secret%29) until explicitly publicized.
* Line 20: Alice [joins](https://docs.reach.sh/ref-model.html#%28tech._join%29) the application by publishing the value to the [consensus network](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29), so it can be used to evaluate the outcome of the game. Once this happens, the code is in a "[consensus step](https://docs.reach.sh/ref-model.html#%28tech._consensus._step%29)" where all participants act together.
* Line 21: Commits the state of the [consensus network](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) and returns to "[local step](https://docs.reach.sh/ref-model.html#%28tech._local._step%29)" where individual participants can act alone.

The next step is similar, in that Bob publishes his hand, however, we won't immediately commit the state, instead we compute the outcome of the game.

[rps-2-rps/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.rsh)

``` js
load: /examples/rps-2-rps/index.rsh
range: 23-29
```

* Lines 23 - 26: Match Alice's similar [local step](https://docs.reach.sh/ref-model.html#%28tech._local._step%29) and [join](https://docs.reach.sh/ref-model.html#%28tech._join%29)ing of the application through a [consensus transfer](https://docs.reach.sh/ref-model.html#%28tech._consensus._transfer%29) [publication](https://docs.reach.sh/ref-model.html#%28tech._publication%29).
* Line 28: Computes the outcome of the game before committing. The equation itself is not critical to understand for the purposes of our tutorial. However, we will say that `Outcome` stores a value of an integer modulo, which will correlate to the winning hand.

In the next snippet we use the [each](https://docs.reach.sh/ref-programs-step.html#%28tech._each%29) form to have each of the participants send the final outcome to their frontends.

[rps-2-rps/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.rsh)

``` js
load: /examples/rps-2-rps/index.rsh
range: 30 - 33
```

* Line 31: States that this is a [local step](https://docs.reach.sh/ref-model.html#%28tech._local._step%29) that [each](https://docs.reach.sh/ref-programs-step.html#%28tech._each%29) of the participants performs.

We can now run the program and see its output running

``` bash
$ reach run
```

Players act randomly so your results may vary. When I ran the program three times, this is the output I received:

``` bash
$ reach run
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ reach run
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ reach run
Alice played Paper
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
```

Alice is good!

## Consensus Networks

[Consensus networks](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) in general, and Reach specifically, guarantee that all participants agree on the outcome of their decentralized computation. Indeed, this is where the name [consensus network](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) comes from, as they enable these distributed, and untrusted, parties to come to a consensus, or agreement, about the intermediate states of a computation. If they agree on the intermediate states, they will also agree on the output. That's why every time you run `reach run`, both Alice and Bob will see the same outcome!

> If your version isn't working, look at the complete versions of [index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.rsh) and [index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-2-rps/index.mjs) to make sure you've copied everything correctly.

> # Who’s on First
> Does Alice go first, or do we call the player that goes first, "Alice"? It may seem like an unnecessary distinction, but this is a subtle point about the way Reach works. In this section, we explicitly ran `backend.Alice` and `backend.Bob` in the [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29). When we did that, we committed that particular JavaScript thread to be either Alice or Bob. In our game, whoever chose to run the Alice backend is the one that will go first. In other words: **Alice goes first**. This will be more obvious at [the end of the tutorial](https://docs.reach.sh/tut-8.html) when we'll make the choice interactively about which role to play.

In the next step, we'll add stakes to the game, because Alice needs to take her skills to the bank!

> # Check Your Understanding
> Reach programs allow interaction with a user interface through which of the following methods?
> 1. By forcing you to write a custom backend for the user interface that connects to the generated smart contract
> 1. By allowing the frontends to provide values directly to the Reach application
> 1. By allowing the Reach program to callback to the frontend via the interact object.

> How do participants in a Reach application share information with each other and find out what others have shared?
> 1. Reach generates a smart contract, but you need to implement a process to scan the blockchain for events that correspond to sharing. 
> 1. The Reach primitive `publish` allows a participant to share information with all other participants, which happens automatically without the other parties needing to do anything special.
> 1. The Reach primitive `publish` allows a participant to share information with all other participants, but they need to explicitly run the receive primitive to receive published information. 

# Bets and Wagers

It's fun to play _Rock, Paper, Scissors!_ with friends for a laugh, but it's even better to play it with enemies and your entire lifesavings on the line! Let's change our program so Alice can offer a wager to Bob and whomever wins will take the pot.

This time, we'll start with changes to the JavaScript [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29) and then we'll go back into the Reach code and connect the new methods.

Since we're going to have funds transferred, we'll record the balances of each participant before the game starts, so we can clearly show what they won at the end. Add this code between account creation and contract deployment. 

[rps-3-bets/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.mjs)

``` js
load: /examples/rps-3-bets/index.mjs
range: 6-13
```

* Line 10: Shows a helpful function to display currency amounts with up to 4 decimal places.
* Line 11: A helpful function for getting the balance of a participant and displaying it with up to 4 decimal places.
* Line 12 & 13: Get the balance before the game starts for Alice and Bob.

## Interface Object

Now, let's update Alice's interface object to include her wager.

[rps-3-bets/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.mjs)

``` js
load: /examples/rps-3-bets/index.mjs
range: 32-35
```

* Line 33: Splices the common `Player` interface into Alice's interface.
* Line 34: Defines her wager as 5 units of the [network token](https://docs.reach.sh/ref-model.html#%28tech._network._token%29). This is an example of using a concrete value, rather than a function, in a [participant interact interface](https://docs.reach.sh/ref-programs-appinit.html#%28tech._participant._interact._interface%29).

Bob's interface will now be modified to show Alice's wager and then immediately accept it. 

[rps-3-bets/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.mjs)

``` js
load: /examples/rps-3-bets/index.mjs
range: 36-41
```

* Lines 38 - 40: Define the `acceptWager` function and print a statement in the console.

Now that the computation is over, we'll get the balance again and show a message that prints the change in funds.

[rps-3-bets/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.mjs)

``` js
load: /examples/rps-3-bets/index.mjs
range: 44 - 48
```

* Lines 44 & 45: Gets the balance after the transaction.
* Lines 47 & 48: Prints the before and after balances.

The changes that we've made only address the presentation and interfacing of the [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29). The business logic and actual wagers and transfers of funds happen in the Reach code.

Let's focus on the backend, now. To begin, we'll update the [participant interact interface](https://docs.reach.sh/ref-programs-appinit.html#%28tech._participant._interact._interface%29).

[rps-3-bets/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.rsh)

``` js
load: /examples/rps-3-bets/index.rsh
range: 1-19
```

* Lines 9 - 12: Defines Alice's interface as the `Player` interface and an integer value, `wager`.
* Lines 13 - 16: Define Bob's interface along with a called `acceptWager`, which can look at the wager value.

## Updating the Application

Next, we'll need to update each of the three parts of the application so it can work with the wager. We'll begin with Alice:

[rps-3-bets/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.rsh)

``` js
load: /examples/rps-3-bets/index.rsh
range: 19-25
```

* Line 20: [Declassifies](https://docs.reach.sh/ref-programs-local.html#%28tech._declassify%29) the wager for transmission on the consensus network. 
* Line 23: Adds the `wager` method so Alice can share her wager with Bob.
* Line 24: Alice transfers her wager as part of her [publication](https://docs.reach.sh/ref-model.html#%28tech._publication%29). If `wager` did not appear in line 23 but did in line 24, then the compiler would throw an exception. _Change the program and try for yourself._ The [consensus network](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) needs to verify that the amount of [network tokens](https://docs.reach.sh/ref-model.html#%28tech._network._token%29) in Alice's publication match the computation available to the consensus network. 

## Accept and Transfer Funds

Next, Bob will be given the ability to be shown the wager, and have the opportunity to accept it, then transfer funds.

[rps-3-bets/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.rsh)

``` js
load: /examples/rps-3-bets/index.rsh
range: 27-32
```

* Line 28: Bob accepts the wager. At this point, if Bob doesn't like the terms, his frontend can simply not respond to this method and the [dApp](https://docs.reach.sh/ref-model.html#%28tech._dapp%29) will stall.
* Line 32: Bob pays the wager

At this point, the dApp is running in a [consensus step](https://docs.reach.sh/ref-model.html#%28tech._consensus._step%29) and the contract itself will now hold twice the wager amount. Previously, it would compute the outcome and then commit the state. Now, it needs to look at the outcome and use it to balance the account.

[rps-3-bets/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-3-bets/index.rsh)

``` js
load: /examples/rps-3-bets/index.rsh
range: 34-41
```

* Lines 35 - 38: Compute the amounts given to each participant depending on the outcome. If the outcome is 2 then Alice wins and she receives two portions of the wager amount. If the outcome is 0 then Bob wins and collects the portions. In a draw, both participants receive 1 portion, each. 
* Lines 39 - 40: Transfer the wagers. Funds reside inside the contract, so the transfer always takes place from the contract to the participants. In this manner, participants do not need to trust one another in order to honestly play the game. 
* Line 41: Commits the state of the application and allows the participants to see the outcome and completes the contract.

We can now run the program and view its output. 

``` bash
$ reach run
```

Remember, players act randomly, so your results may vary. This is the output I received from three playthroughs:

``` bash
Alice played Paper
Bob accepts the wager of 5.
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 100 to 104.9999.
Bob went from 100 to 94.9999.

$ reach run
Alice played Paper
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.

$ reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 100 to 104.9999.
Bob went from 100 to 94.9999.
```

Alice is playing strong. Soon she'll have a fortune from playing _Rock, Paper, Scissors!_

You may have noticed that Alice and Bob always return to a balance of 100. This is because `reach run` creates fresh accounts for both players. Remember, we've hard coded a balance of 100 network tokens for this tutorial.

Don't deploy this game on the mainnet! It turns out that now we have an incentive to play that there is a major security vulnerability. Let's fix this before Alice goes broke from a dishonest Bob!

> # Check Your Understanding
> How do Reach programs manage token funds?
> 1. They don't; you need to manage them explicitly in parallel to the Reach program
> 1. The pay primitive can be added to a publish primitive to send funds to the Reach program, which can then use the transfer primitive to send funds back to participants, and other addresses.

# Trust, Commitments, and Attacks

Alice and Bob are now able to exchange currency, but there is a fundamental flaw in our program: Bob is able to win every game!

How so, when we've seen Alice win so many games?

The problem is that we've only executed an [honest](https://docs.reach.sh/ref-model.html#%28tech._honest%29) version of Bob. However, it's possible for a dishonest Bob to execute different code in his [backend](https://docs.reach.sh/ref-model.html#%28tech._backend%29) so that he always wins by computing the winning hand based on information Alice shares in `handAlice`.

Let's give Bob a dishonest advantage:

[rps-4-attack/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-4-attack/index.rsh#L27-L32)

``` js
load: /examples/rps-4-attack/index.rsh
range: 27-32
```

Now Bob will ignore the frontend and compute the correct value. 

Running the program will return an output similar to this:

``` bash

$ reach run
Alice played Scissors
Bob accepts the wager of 5.
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.
```

We can see from the output that Bob never consults the frontend. Bob will never print his hand and he will always win. 

## Automatic Verification Engine

How do we know it's not just luck of the random number generator that observes Bob winning? Reach comes with an automatic verification engine that mathematically proves that this version always results in the `outcome` equaling 0, meaning Bob wins. We can prove this theorem with the `require` and `assert` statements after computing the `outcome`.

[rps-4-attack/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-4-attack/index.rsh#L34-L37)

``` js
load: /examples/rps-4-attack/index.rsh
range: 34-37
```

* Line 35: Requires that the dishonest version of Bob be used for the proof.
* Line 36: Conducts the proof by including an [assert](https://docs.reach.sh/ref-model.html#%28tech._assert%29) statement in the program.

Now our verification engine checks an additional 5 theorems and prints this when compiled:

``` bash
..    // ...
 2    Verifying for generic connector
 3      Verifying when ALL participants are honest
 4      Verifying when NO participants are honest
 5      Verifying when ONLY "Alice" is honest
 6      Verifying when ONLY "Bob" is honest
 7    Checked 23 theorems; No failures!
```

At compile time, Reach conducts a mathematical proof that the expression always evaluates to `true`. We use the [verification engine](https://docs.reach.sh/guide-assert.html) to prove that an attack would do what we expect. But it's better to use verification to show that _no flaws_ exist and that _no attack_ is possible.

Reach will check a certain number of theorems even without explicit `require` and `assert` statements. Let's see these theorems in action by inserting an error into the program.

First, undo the recent changes we made. So, we'll remove the `require` and `assert` statements and return `handBob` to its previous expression.

```js
29        const handBob = declassify(interact.getHand());
```

Find the payout computation and alter it so that when Alice wins, she only gets her wager back, not Bob's, as well.

```js
34      const outcome = (handAlice + (4 - handBob)) % 3;
35      const            [forAlice, forBob] =
36        outcome == 2 ? [       1,      0] : // <- Oops. was: [2, 0]
37        outcome == 0 ? [       0,      2] :
38        /* tie      */ [       1,      1];
39      transfer(forAlice * wager).to(Alice);
40      transfer(forBob * wager).to(Bob);
41      commit();
```

* Line 36 is now [1, 0].

Running `$ reach compile` will share details about the error:

``` bash
..    
 4    Verification failed:
 5      when ALL participants are honest
 6      of theorem: assert
 7      msg: "balance zero at application exit"
 8      at ./index-bad.rsh:8:30:compileDApp
 9    
10      // Violation Witness
11    
12      const v71 = "Alice".interact.wager;
13      //    ^ could = 1
14      //      from: ./index-bad.rsh:11:10:property binding
15      const v74 = protect<UInt>("Alice".interact.getHand());
16      //    ^ could = 0
17      //      from: ./index-bad.rsh:21:50:application
18      const v84 = protect<UInt>("Bob".interact.getHand());
19      //    ^ could = 2
20      //      from: ./index-bad.rsh:29:48:application
21    
22      // Theorem Formalization
23    
24      const v93 = (v74 + (4 - v84)) % 3;
25      //    ^ would be 2
26      const v100 = (v93 == 2) ? [1, 0 ] : (v93 == 0) ? [0, 2 ] : [1, 1 ];
27      //    ^ would be [1, 0 ]
28      const v114 = 0 == (((v71 + v71) - (v100[0] * v71)) - (v100[1] * v71));
29      //    ^ would be false
30      assert(v114);
31    
..    
```

There's a lot of information here that you'll be able use with some experience. For now, we'll choose to focus on the most important parts:

* Line 7 states an attempt to prove that the balance at the end of the program is zero. This is important because a balance left at the end of a program will result in tokens being sealed in the [contract](https://docs.reach.sh/ref-model.html#%28tech._contract%29) forever. 
* Lines 10 - 20: Describe values that will cause the theorem to fail.
* Lines 23 - 31: Detailed the theorem that failed.

The [automatic verifications](https://docs.reach.sh/guide-assert.html) help Reach programmers by protecting them from entire categories of errors.

## Assertions

Now, let's add an [assert](https://docs.reach.sh/ref-model.html#%28tech._assert%29)ion that rejects a dishonest Bob. We'll rewind our code to the end of the Bets and Wagers section. In this version Bob is still honest. To help keep Bob honest, we'll add a single line after Alice publishes, but before Bob's [local step](https://docs.reach.sh/ref-model.html#%28tech._local._step%29):

``` js
..    // ...
23    Alice.publish(wager, handAlice)
24      .pay(wager);
25    commit();
26    
27    unknowable(Bob, Alice(handAlice));
28    Bob.only(() => {
29      interact.acceptWager(wager);
30      const handBob = declassify(interact.getHand());
31    });
..    // ...
```

* Line 27: Contains a knowledge assertion that Bob cannot know Alice's value in `handAlice`. Looking at line 23, we can see that this is not true. However, in most cases, it won't be so obvious. 

Running `$ reach run` will report that this assertion is false:

``` bash
..    
 2    Verification failed:
 3      of theorem unknowable("Bob", handAlice/77)
 4      at ./index-fails.rsh:27:13:application
 5    
 6      Bob knows of handAlice/77 because it is published.
..   
```

It's not enough to correct failures and attacks as they are discovered. **Always** add asserts to your program where a failure or an attack would create an undesirable outcome. This will ensure that all similar attacks are not present or accidentally reintroduced.

## A More Secure Version

With this knowledge in mind, let's create a brand-new version of _Rock, Paper, Scissors!_ so that it is more trustworthy and secure. 

Let's begin by defining the rules of _Rock, Paper, Scissors!_ with a greater amount of abstraction. This will allow us to separate the logic of the game from the details of the application.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L1-L7)

``` js
load: /examples/rps-5-trust/index.rsh
range: 1-7
```

* Line 1: The Reach version header in all reach files
* Lines 3 - 4: Define [enumeration](https://docs.reach.sh/ref-programs-compute.html#%28tech._enumeration%29)s for the hands that may be played, as well as the outcomes of the game. 
* Lines 6 - 7: Defines the function that computes the winner of the game.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L9-L11)

``` js
load: /examples/rps-5-trust/index.rsh
range: 9-11
```

* Lines 9 - 11: Test cases that assert a winner or a draw. This ensures that despite the values found in `handAlice` and `handBob`, `winner` will always be a valid outcome.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L13-L15)

``` js
load: /examples/rps-5-trust/index.rsh
range: 13-15
```

Lines 13 - 15: Specifies that whenever the same value is provided for both hands that the `winner` value is `DRAW`.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L17-L18)

``` js
load: /examples/rps-5-trust/index.rsh
range: 17-18
```

* Line 17: `forall` quantifies over all possible inputs for `hand`.
* Line 18: `assert` states that if the hand values are the same then the outcome is equal to `DRAW`.

Next, we'll specify the participant interact interface for Alice and Bob.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L20-L24)

``` js
load: /examples/rps-5-trust/index.rsh
range: 20-24
```

* Line 21: Provides the frontend access to random numbers. This will be used to protect Alice's hand from a dishonest Bob.

[rps-5-trust/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.mjs#L20-L30)

``` js
load: /examples/rps-5-trust/index.mjs
range: 20-30
```

* Line 21: Allows each participant to generate random numbers

## Randomness

These two changes might look identical, but they mean very different things. The first, line 21 in the Reach program, adds `hasRandom` to the interface that the [backend](https://docs.reach.sh/ref-model.html#%28tech._backend%29) expects the [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29) to provide. The second, line 21 in the JavaScript, adds [hasRandom] to the implementation that the frontend provides to the backend.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L25-L36)

``` js
load: /examples/rps-5-trust/index.rsh
range: 25-36
```

There is nothing new or exciting in this snippet. We're creating the Reach application and providing functionality to the participant interact interface.

In the next snippet, we'll implement the application while ensuring that Alice's hand is protected until Bob reveals his hand. We could just have Alice publish her wager, but then this would leave Bob vulnerable. Our challenge is that we need Alice to publish her hand, but also keep it secret. Our solution is to use a [cryptographic commitment scheme](https://en.wikipedia.org/wiki/Commitment_scheme). Reach’s standard library comes with `makeCommitment` to make this easier.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L37-L45)

``` js
load: /examples/rps-5-trust/index.rsh
range: 37-45
```

* Line 39: Alice computes her hand but does not declassify it.
* Line 40: Alice computes a commitment to her hand. We've added a secret "salt" value that was generated by the `random` function, which we'll reveal later.
* Line 41: Alice declassifies the commitment.
* Line 43: Alice publishes the commitment.
* Line 44: Includes Alice's wager funds in the publication.

Next, we'll state the [knowledge assertion](https://docs.reach.sh/ref-model.html#%28tech._knowledge._assertion%29) that Bob can't know either the hand or the "salt" and continue with his part of the program.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L47-L54)

``` js
load: /examples/rps-5-trust/index.rsh
range: 47-54
```

* Line 47: States the [knowledge assertion](https://docs.reach.sh/ref-model.html#%28tech._knowledge._assertion%29).
* Lines 48 - 53: These lines are unchanged from the original version.
* Line 54: Transaction commit method. The payout isn't computed yet because Alice's hand is not currently public.

Alice is ready to reveal her secrets so let's return our focus to her.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L56-L61)

``` js
load: /examples/rps-5-trust/index.rsh
range: 56-61
```

Lines 57 - 58: Alice declassifies the secret information.
Line 60: Alice publishes the secret information.
Line 61: Checks that the published values match the original values. This will always be true for honest participants, but dishonest participants may violate this assumption.

The rest of the program is unchanged from the original version, except that it uses the new names for the outcomes.

[rps-5-trust/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-5-trust/index.rsh#L63-L74)

``` js
load: /examples/rps-5-trust/index.rsh
range: 63-74
```

We didn't change the frontend in a meaningful way so the output from `reach run` looks the same as before.

``` bash
$ reach run
Alice played Scissors
Bob accepts the wager of 5.
Bob played Paper
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 100 to 104.9999.
Bob went from 100 to 94.9999.

$ reach run
Alice played Paper
Bob accepts the wager of 5.
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.

$ reach run
Alice played Scissors
Bob accepts the wager of 5.
Bob played Scissors
Bob saw outcome Draw
Alice saw outcome Draw
Alice went from 10 to 9.9999.
Bob went from 10 to 9.9999.
```

While the frontend looks the same, we know that behind the scenes, Alice now takes two steps in the Reach program and Bob only takes one. Alice is protected against Bob finding her hand and using it for his advantage.

You may have noticed that this version of the application proves more theorems which help protect Reach programs from a variety of human-errors. Non-Reach decentralized programmers are on their own when trying to ensure that these problems don't exist. 

Our implementation of _Rock, Paper, Scissors!_ is secure and free of exploits for Alice and Bob. However, there is a final category of mistakes that is common in dApps: non-participation. 

As before, don't launch this version of _Rock, Paper, Scissors!_ in the mainnet or Alice may ghost Bob when she knows she's going to lose!

> # Check Your Understanding
> True or false: Since blockchain programs run on a single, global, publicly checked and certified consensus network, you don't need to test them as much as normal software, which run on a wide variety of different platforms and operating systems. 

> True or false: It is easy to write correct programs that handle financial information, and even if you make a mistake, blockchains support an "Undo" operation that allows you to rollback to earlier versions of the leader to correct mistakes and recover lost funds.

> True or false: Reach provides automatic verifications to ensure that your program does not lose, lock away, or overspend funds and guarantees that your applications are free from entire categories of errors. 

> True or false: Reach provides tools for you to add custom verifications to your program, like ensuring that information is known only to one party, or that your implementation of a sensitive algorithm is correct.

# Timeouts and Participation

We've just removed a major security vulnerability. Way to avoid a scandal in the press! In this section, we'll focus on a subtle issue that is important and unique to decentralized applications: [non-participation](https://docs.reach.sh/guide-timeout.html).

Non-participation is the act of one party ceasing to continue playing their role in an application.

In traditional client-server programs this would be the equivalent of a client not sending additional requests to the server, or the server ceasing to send responses to the client. In centralized apps, non-participation is an exceptional circumstance that often leads to an error message for clients or a log entry for servers. Traditionally, developers have not needed to meticulously consider the consequences of non-participation. 

However, in decentralized applications engineers must carefully consider the behavior of their dApp in the event of non-participation. Imagine if Alice pays her wager, but Bob never accepts. The application will not continue and Alice's [network tokens](https://docs.reach.sh/ref-model.html#%28tech._network._token%29) will be locked away in the [contract](https://docs.reach.sh/ref-model.html#%28tech._contract%29) forever. That wouldn't be too smart, right?

We can also imagine a scenario where Bob accepts and pays his wager, but Alice never submits her hand, locking away both of their funds forever. In either case, both parties would be hurt, and their fear of non-participation would increase the costs of each transaction and reduce the value of interacting with that contract.

Of course, for our tutorial, this is a trivial matter, but as a microcosm of decentralized application design, it is a consideration of extreme importance. Let's explore how Reach helps to address non-participation. 

## Mitigating Non-Participation

In Reach, non-participation is mitigated through a "timeout" mechanism that pairs each [consensus transfer](https://docs.reach.sh/ref-model.html#%28tech._consensus._transfer%29) with a [step](https://docs.reach.sh/ref-model.html#%28tech._step%29) that occurs for all [participants](https://docs.reach.sh/ref-model.html#%28tech._participant%29) if the [originator](https://docs.reach.sh/ref-model.html#%28tech._originator%29) of the [consensus transfer](https://docs.reach.sh/ref-model.html#%28tech._consensus._transfer%29) fails to make the required [publication](https://docs.reach.sh/ref-model.html#%28tech._publication%29) before a particular network time.

Let's modify the [participant interact interface](https://docs.reach.sh/ref-programs-appinit.html#%28tech._participant._interact._interface%29) to allow the [frontend](https://docs.reach.sh/ref-model.html#%28tech._frontend%29) to be informed in the event of a timeout.

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L20-L25)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 20-25
```

* Line 24: introduces a new method, `informTimeout`, which receives no arguments and returns no information. This function is called when a timeout occurs. 

Now let's update the JavaScript frontend so it can receive the message and display it in the console. 

[rps-6-timeouts/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.mjs#L20-L33)

``` js
load: /examples/rps-6-timeouts/index.mjs
range: 20-33
```

* Lines 30 - 32: The `informTimeout` object has been added with a message notifying the user that a timeout has occurred. 

We'll return to the Reach program and declare a deadline value. Alice will provide the deadline similar to how she provides the wager.

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L28-L32)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 28-32
```

* Line 31: `deadline` has been added to Alice's participant interact interface. 

Next, we'll define a helper function to inform each of the participants of the timeout.

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L39-L43)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 39-43
```

* Line 39: Defines an [arrow expression](https://docs.reach.sh/ref-programs-compute.html#%28tech._arrow._expression%29) called `informTimeout`
* Line 40: Has each of the participants perform a [local step](https://docs.reach.sh/ref-model.html#%28tech._local._step%29).
* Line 41: Has each participant call the `informTimeout` method.

Our next step is to have Alice declassify and publish the `deadline` to use in future [timeout](https://docs.reach.sh/ref-model.html#%28tech._timeout%29) clauses.

We won't add a timeout clause to Alice's first message because there's no consequence to her non-participation. She doesn't start the game, then no one is any worse off!

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L45-L54)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 45-54
```

* Line 50: Alice declassifies the `deadline` 
* Line 51: Alice publishes the `deadline`.

Bob's non-participation is different in nature than Alice's first opportunity for non-participation. If he fails to participate then Alice loses her initial wager. With this in mind, let's adjust Bob's first message.

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L61-L64)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 61-64
```

* Line 63 adds a timeout handler to Bob's [publication](https://docs.reach.sh/ref-model.html#%28tech._publication%29).

## Timeout Handler

The timeout handler specifies that if the deadline is breached then the program will transition to the next step. In this case, the next step is to publish a message in the console and return the network tokens to Alice.

Next, we'll add a similar timeout handler to Alice's second message.

[rps-6-timeouts/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L70-L71)

``` js
load: /examples/rps-6-timeouts/index.rsh
range: 70-71
```

* Lines 70 - 71: This is like our `timeout` handler in Bob's publication, but in this case, if Alice triggers the `timeout` event then Bob will collect all the funds. 

While it might be "fair" for Alice and Bob to each receive their own funds, this type of implementation would incentivize Alice to always timeout if she were going to lose, which she will know, because she'll know her hand and Bob's hand.

Our Reach program is now robust against non-participation attacks with a minimal addition of eleven lines!

For the purposes of our tutorial, we'll now modify the frontend to purposely cause a random timeout when Bob is supposed to accept the wager. 

[rps-6-timeouts/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-6-timeouts/index.rsh#L35-L54)

``` js
load: /examples/rps-6-timeouts/index.mjs
range: 35-54
```

* Line 39: Alice specifies a `deadline` of ten blocks
* Lines 43 - 52: redefine Bob's `acceptWager` method as an asynchronous function. 
* Lines 44 - 46: Now, half of the time Bob will trigger a timeout event.

## Run with Timeouts

Let's observe two possible outcomes:

``` bash
$ reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.

$ reach run
Alice played Scissors
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
  Bob takes his sweet time...
Bob played Scissors
Bob observed a timeout
Alice observed a timeout
Alice went from 10 to 9.9999.
Bob went from 10 to 9.9999.
```

We've now created a robust dApp that prevents either participant from dropping the game. If you'd like to dig into a deeper discussion on this subject, then visit our [guide on non-participation](https://docs.reach.sh/guide-timeout.html). In the next iteration, we'll extend the application to disallow draws. 

> # Check Your Understanding
> What happens in a decentralized application when one participant refuses to take the next step of the program? 
1. This is not possible, because the blockchain guarantees that each party performs a particular set of actions
1. The program hangs forever waiting for a participant to provide a value
1. The program punishes the non-participant and proceeds as if the active participant won.
1. It depends on how the program was written; if the developer used Reach, the default is (2), but the developer could include a `timeout` block to implement the behavior in #3.

# Play and Play Again

Our program is coming together, now, but it's not as fun to end on a draw. We can extend the application so Alice and Bob play until a winner is declared. 

While this refactor will only require a change to the Reach program, we will also take the opportunity to modify the frontend so timeouts can happen to either party when they are asked to submit their hands. Let's do that now and then we'll focus on removing the ability to end on a draw.

We'll begin by modifying the `Player` interact object so that it will have a different `getHand` method.

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L20-L39)

``` js
load: /examples/rps-7-loops/index.mjs
range: 20-39
```

* Lines 25 - 30: Moves the forced timeout code that we wrote for Bob's `acceptWager` function into the `getHand` method. We've also changed the threshold, so timeouts only happen 1% of the time. 

Next, we'll adjust Bob's `acceptWager` function to remove the timeout code.

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L41-L53)

``` js
load: /examples/rps-7-loops/index.mjs
range: 41-53
```

* Lines 49 - 51: The `acceptWager` method has been simplified.

## Order of Actions

Let's step back and look at our game's original steps and our new order of actions.

Previously, our steps were:

1. Alice sends her wager and commitment.
1. Bob accepts the wager and sends his hand.
1. Alice reveals her hand.
1. The game ends.

But now because players might submit many hands, but should only have a single wager, we'll break these steps up differently:

1. Alice sends her wager.
1. Bob accepts the wager.
1. Alice sends her commitment.
1. Bob sends his hand.
1. Alice reveals her hand.
1. If the outcome is draw, return to step 3; otherwise, the game ends.

Let's make these changes now.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L45-L51)

``` js
load: /examples/rps-7-loops/index.mjs
range: 45-51
```

* Line 49: Alice publishes the wager and deadline.
* Line 50: Alice pays the wager.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L53-L58)

``` js
load: /examples/rps-7-loops/index.rsh
range: 53-58
```

* Line 56: Bob pays the wager.
* Line 58: Does **not** have the previously implemented [consensus step](https://docs.reach.sh/ref-model.html#%28tech._consensus._step%29) commit.

## Loop Invariants

It's now time to begin the repeatable section of the application. Each party will repeatedly submit hands until the outcome is not a draw. In normal programming languages, such a circumstance would be implemented with a `while` loop, which is exactly what we'll do in Reach. However, `while` loops in Reach require extra care. We'll take our time working through loops in this tutorial. Upon your completion of the tutorial, you can learn more about loops in our [guide](https://docs.reach.sh/guide-loop-invs.html).

So far, our identifier bindings have been static and unchangeable. However, if this were the case with while loops, they would never start or terminate, because the loop condition would never change. So, we bind `while` loops using `var`.

Due to Reach's [automatic verification](https://docs.reach.sh/guide-assert.html) engine, we need to make a statement about what properties of the program are invariant before and after a `while` loop body's execution. This is known as a "[loop invariant](https://docs.reach.sh/guide-loop-invs.html)."

It's important to note that these loops _may only occur_ inside [consensus steps](https://docs.reach.sh/ref-model.html#%28tech._consensus._step%29). This is why we didn't commit Bob's transaction, previously. We need to remain inside the consensus to start the `while` loop, because all [participants](https://docs.reach.sh/ref-model.html#%28tech._participant%29) must agree on the direction of control flow in the dApp.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L59-L61)

``` js
load: /examples/rps-7-loops/index.rsh
range: 59-61
```

* Line 59: Defines the loop variable, `outcome`
* Line 60: States the invariant as the body of the loop does not change the balance in the [contract](https://docs.reach.sh/ref-model.html#%28tech._contract%29) account and that `outcome` is a valid outcome.
* Line 61: Begins the loop with the condition that it continues as long as the outcome is a draw.

Now, we'll create the body of the loop, beginning with Alice's commitment to her hand. 

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L61-L71)

``` js
load: /examples/rps-7-loops/index.rsh
range: 61-71
```

* Line 62: Commits the last transaction. At the beginning of the loop, this commits Bob's acceptance of the wager, and in subsequent runs, publishes Alice's hand.
* Line 64 - 71: Similar to before, but now the wager is already known and paid.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L73-L79)

``` js
load: /examples/rps-7-loops/index.rsh
range: 73-79
```

Bob's code is almost identical to the previous version, except that he's already accepted and paid the wager.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L81-L87)

``` js
load: /examples/rps-7-loops/index.rsh
range: 81-87
```

Alice's next step is identical, because she is still revealing her hand in the same way.

Finally, we write the end of the loop:

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L89-L91)

``` js
load: /examples/rps-7-loops/index.rsh
range: 89-91
```

* Line 89: Updates the `outcome` loop variable with the new value.
* Line 90: Continues the loop. Unlike most programming languages, Reach **requires** that `continue` be _explicitly_ written in the loop body.

The rest of the program could be the same as before, except it now occurs outside of the loop. We'll make one additional simplification because we now know that the outcome can never be a draw.

[rps-7-loops/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.rsh#L93-L100)

``` js
load: /examples/rps-7-loops/index.rsh
range: 93-100
```

* Line 93: Asserts the outcome is never "DRAW".
* Line 94: Transfers the funds to the winner.
* Line 95: Commits to the consensus network.
* Lines 97 - 99: Allows Alice and Bob to interact with the contract and see the outcome of the game.

## Run Without Draws

Let's run the program to witness our loop in action:

``` bash
$ reach run
Bob accepts the wager of 5.
Alice played Paper
Bob played Rock
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 100 to 104.9999.
Bob went from 100 to 94.9999.

$ reach run
Bob accepts the wager of 5.
Alice played Rock
Bob played Rock
Alice played Paper
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.
 
$ reach run
Bob accepts the wager of 5.
Alice played Scissors
Bob played Rock
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 100 to 94.9999.
Bob went from 100 to 104.9999.
```

Your results will likely differ, but with enough runs, you should be able to see single round, and multi-round battles. Statistically, 1 out of every 100 games should result in a timeout, as well.

This implementation of _Rock, Paper, Scissors!_ will always result in a pay-out, which is more exciting for every participant. In the next section, we'll learn how to exit "testing" mode with Reach and turn the JavaScript into an interactive _Rock, Paper, Scissors!_ game with real users.

> # Check Your Understanding
> How do you write an application in Reach that runs arbitrarily long, like a game of Rock, Paper, Scissors that is guaranteed to not end in a draw?
1. This is not possible, because all Reach programs are finitely long
1. You can use a `while` loop that runs until the outcome of the game is decided.

> When you check if a program with a `while` loop is correct, you need to have a property called a loop invariant. Which of the following statements must be true about the loop invariant?
1. The part of the program before the `while` loop must establish the invariant.
1. The condition and the body of the loop must establish the invariant.
1. The negation of the condition and the invariant must establish any properties of the rest of the program.

# Interaction and Independence

Previously, we eliminated the possibility of a DRAW in our Reach program and made very few changes in the JavaScript frontend. In this section, we won't make any changes to the backend. Instead, we'll customize the frontend to facilitate interactivity and provide the option to connect to a real consensus network.

## Start from Scratch

We'll start from scratch and show every line of the program, again. While much of the code will be like the previous version, for the sake of completeness, we'll show every line.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L1-L6)

``` js
load: /examples/rps-8-interact/index.mjs
range: 1-6
```

* Lines 1, 2, & 4: Are the same as before; importing the standard library and backend.
* Line 3: Imports a helpful library for simple console applications called `ask.mjs` from the Reach standard library. We'll see how these three functions are used below.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L7-L12)

``` js
load: /examples/rps-8-interact/index.mjs
range: 7-12
```

* Lines 7 - 10: Asks whether the user is playing as Alice and expects a "Yes" or "No" answer. `ask` presents a prompt and collects a line of input until its argument does not error. `yesno` errors if it is not given "y" or "n".

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L13-L29)

``` js
load: /examples/rps-8-interact/index.mjs
range: 13-29
```

* Lines 16 - 19: Presents the user with the choice of creating a test account or inputting a secret to load an existing account.
* Line 21: Creates the test account.
* Line 27: Loads the existing account.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L30-L46)

``` js
load: /examples/rps-8-interact/index.mjs
range: 30-46
```

* Lines 31 - 34: Ask if the participant will deploy the contract.
* Lines 36 - 38: deploy it and print out public information (`ctc.getInfo`) that can be given to the other player when it becomes available.
* Lines 40 - 44: Request, parse, and process the information.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L47-L54)

``` js
load: /examples/rps-8-interact/index.mjs
range: 47-54
```

Next, we define a few helper functions and start the participant interaction interface.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L55-L59)

``` js
load: /examples/rps-8-interact/index.mjs
range: 55-59
```

* Lines 55 - 58: Define a timeout handler

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L60-L78)

``` js
load: /examples/rps-8-interact/index.mjs
range: 60-78
```

Next, we request the wager amount or define the `acceptWager` method, depending on if we are Alice or not.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L79-L97)

``` js
load: /examples/rps-8-interact/index.mjs
range: 79-97
```

Lines 86 - 96: Defines the shared `getHand` method.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L98-L102)

``` js
load: /examples/rps-8-interact/index.mjs
range: 98-102
```

* Lines 98 - 101: Creates the `seeOutcome` method.

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs#L103-L110)

``` js
load: /examples/rps-8-interact/index.mjs
range: 103-110
```

Line 103: Chooses the appropriate backend function and awaits its completion.

## Run in Two Terminals

We can now open two terminals. Run the first as Alice and the second as Bob:

``` bash
$ reach run
Are you Alice?
y
Starting Rock, Paper, Scissors as Alice
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the contract? (y/n)
y
The contract is deployed as = "0x132b724e55AEb074C15A5CBb7b8EeE0dBEd45F7b"
Your balance is 999.9999
How much do you want to wager?
10
What hand will you play?
r
You played Rock
The outcome is: Bob wins
Your balance is now 989.9999
```

and for Bob:

``` bash
$ reach run
Are you Alice?
n
Starting Rock, Paper, Scissors as Bob
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the contract? (y/n)
n
Please paste the contract information:
"0x132b724e55AEb074C15A5CBb7b8EeE0dBEd45F7b"
Your balance is 1000
Do you accept the wager of 10?
y
What hand will you play?
p
You played Paper
The outcome is: Bob wins
Your balance is now 1009.9999
```

When you run your versions of Alice and Bob your exact amounts and addresses may be different.

If we wanted to run our program on the Algorand devnet then we'd set the Reach connector mode with the following command in both terminals:

``` bash
$ REACH_CONNECTOR_MODE=ALGO-devnet reach run
```

Connecting to live [consensus networks](https://docs.reach.sh/ref-model.html#%28tech._consensus._network%29) is similarly easy:

``` bash
$ REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI="http://some.node.fqdn:8545" reach run
```

## Completing the Command-Line Interface

Our implementation of _Rock, Paper, Scissors!_ is now finished! We are protected against attacks, timeouts, and draws and can run interactively on non-test networks. 

In this step we made a command-line interface for each participant. In the next section, we'll create a web interface.

> # Check Your Understanding
> Reach helps you build automated tests for your decentralized application, but it doesn't support building interactive user-interfaces.

# Web Interaction with React

Just as before, we won't be making any changes to the backend Reach program. Instead, we're going to focus on replacing the command-line interface with a web interface. 

We'll use [React.js](https://reactjs.org/) for this tutorial, but the same principles apply to any web framework.

## React.js Basics

If you’ve never used React before, here are some basics about how it works:
* React programs are JavaScript programs that use a special library that allows you to mix HTML inside of the body of your JavaScript.
* React has a special compiler that combines a bundle of JavaScript programs, and all their dependencies, into one large file that can be deployed on a static Web server. This is called "packing".
* When you’re developing and testing with React, you run a special development Web server that watches and updates this packed file every time you modify a source file, so you don’t have to constantly run the compiler.
* Reach automates the process of starting this development server for you when you run ./reach react and gives you access to it at http://localhost:3000/.

## Deploying

This tutorial assumes we're deploying and testing with Ethereum. Reach web applications rely on the web browser to provide access to a consensus network account and its associated wallet. Ethereum’s standard wallet is [MetaMask](https://metamask.io/). Install and set it up if you want to test this code. 

> Note that MetaMask does not support multiple active accounts, so testing locally will require the use of two separate browser instances with two MetaMask accounts: one to act as Alice and the other as Bob.

To complete this section, we'll use the `index.rsh` file you've already written but create an `index.js` file from scratch. This will replace the `index.mjs` file we wrote for the command-line interface.

This code is supplemented with [index.css](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.css) and some [views](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/views). These details are not specific to Reach, and are trivial, so we will not explain the specific of those files. If you run this locally, you'll want to download those file. Your directory should look like:

``` bash
.
├── index.css
├── index.js
├── index.rsh
└── views
    ├── AppViews.js
    ├── AttacherViews.js
    ├── DeployerViews.js
    ├── PlayerViews.js
    └── render.js
```

## React Frontend

Let's begin writing the new JavaScript frontend. 

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L1-L9)

``` js
load: /examples/rps-9-web/index.js
range: 1-9
```

* Lines 1 - 6: Import the views and CSS.
* Line 7: Import the compiled `backend`
* Lines 8 & 9: Load the `stdlib` as `reach`.

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L10-L14)

``` js
load: /examples/rps-9-web/index.js
range: 10-14
```

Here we've defined helpful constants and defaults for later use.

## Application Component

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L15-L31)

``` js
load: /examples/rps-9-web/index.js
range: 15-31
```

Here, we've begun defining `App` as a React component and provide instructions for what to do once React mounts (i.e., when React starts).

* Line 19: We initialize the component state to display the `ConnectAccount` view. (As pictured below).
* Lines 21 - 31: Hooks into React's `componentDidMount` lifecycle event, which is called when the component starts.
* Line 22: We use `getDefaultAccount`, which accesses the default browser account. (i.e., MetaMask)
* Line 26: We use `canFundFromFaucet` to check if we can access the Reach developer testing network faucet.
* Line 27: If `canFundFromFaucet` is `true`, we set the component state to display the `FundAccount` view.
* Line 29: If `canFundFromFaucet` is `false`, we set the component state to skip to the `DeployerOrAttacher` view. 

## Connect Account Dialog

![The `ConnectAccount` view](ConnectAccount.png)

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L39-L41)

``` js
load: /examples/rps-9-web/index.js
range: 39-41
```

* Line 39: We render the appropriate view from [rps-9-web/views/AppViews.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/views/AppViews.js)

![Figure 2: The `FundAccount` view](FundAccount.png)

We'll return to the close of `componentDidMount` to define callbacks on `App` for what to do when the user clicks certain buttons.

## Fund Account Dialog

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L32-L36)

``` js
load: /examples/rps-9-web/index.js
range: 32-36
```

* Lines 32 - 35: Define what happens when the user clicks the `Fund Account` button.
* Line 33: Transfer funds from the faucet to the user's account.
* Line 34: Sets the component state to display the `DeployerOrAttacher` view. (figure 3).
* Line 36: Define what to do when the user clicks the `Skip` button, which is to set the component state to display the `DeployerOrAttacher` view.

## Choose Role

![Figure 3: The `DeployerOrAttacher` view](DeployerOrAttacher.png)

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L36-L38)

``` js
load: /examples/rps-9-web/index.js
range: 36-38
```

* Lines 37 & 38: Sets a sub-component based on whether the user clicks `Deployer` or `Attacher`.

## Player Component

Next, we define `Player` as a React component, which will be extended by the specialized components for Alice and Bob. The web frontend needs to implement the participant interact interface for players, which we previously defined as:

[rps-9-web/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.rsh#L20-L25)

``` js
load: /examples/rps-9-web/index.rsh
range: 20-25
```

We will provide these callbacks via the React component directly:

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L42-L55)

``` js
load: /examples/rps-9-web/index.js
range: 42-55
```

* Line 43: Provides the `random` callback.
* Lines 44 - 50: Provides the `getHand` callback.
* Lines 45 - 47: Sets the component state to display the `GetHand` view (figure 4) and wait for a `Promise` which can be resolved via user interaction.
* Line 48: Occurs after the `Promise` is resolved, we set the component state to display the `WaitingForResults` view (figure 5).
* Lines 51 & 52: Provides the `seeOutcome` and `informTimeout` callbacks, which set the component state to display the `Done` view (figure 6) and the `Timeout` view (figure 7), respectively.
* Line 53: Defines what happens when the user clicks `Rock`, `Paper`, `Scissors`: The `Promise` from line 45 is resolved.

## Get Hand Dialog

![Figure 4: The `GetHand` view](GetHand.png)

## Waiting for Results Display

![Figure 5: The `WaitingForResults` view](WaitingForResults.png)

## Done Display

![Figure 6: The `Done` view](Done.png)

## Timeout Display

![Figure 7: The `Timeout` view](Timeout.png)

## Deployer Component

Next, our web frontend needs to implement the participant interact interface for Alice, which we previously defined as:

[rps-9-web/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.rsh#L28-L32)

``` js
load: /examples/rps-9-web/index.rsh
range: 28-32
```

We provide the `wager` and `deadline` values and define some button handlers in order to trigger the deployment of the contract. 

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L56-L72)

``` js
load: /examples/rps-9-web/index.js
range: 56-72
```

* Line 59: Sets the component state to display the `SetWager` view (figure 8).
* Line 61: Defines what to do when the user clicks the `Set Wager` button, which is to set the component state to display the `Deploy` view (figure 9).
* Lines 62 - 69: Defines what to do when the user clicks the `Deploy` button.
* Line 63: Calls `acc.deploy`, which triggers a deploy of the contract.
* Line 64: Sets the component state to display the `Deploying` view (figure 10).
* Line 65: Sets the `wager` property.
* Line 66: Sets the `deadline` property based on which connector is being used.
* Line 67: Starts running the Reach program as Alice, using the `this` React component as the participant interact interface object.
* Lines 68 & 69: Sets the component state to display the `WaitingForAttacher` view (figure 11), which displays the deployed contract info as JSON.
* Line 71: Renders the appropriate view from [rps-9-web/views/DeployerViews.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/views/DeployerViews.js).

## Set Wager Dialog

![Figure 8: The `SetWager` view](SetWager.png)

## Deploy Dialog

![Figure 9: The `Deploy` view](Deploy.png)

## Deploying Display

![Figure 10: The `Deploying` view](Deploying.png)

## Waiting for Attacher Display

![Figure 11: The `WaitingForAttacher` view](WaitingForAttacher.png)

## Attacher Component

The frontend needs to implement the participant interact interface for Bob, which we previously defined as:

[rps-9-web/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.rsh#L33-L36)

``` js
load: /examples/rps-9-web/index.rsh
range: 33-36
```

Next, we'll provide the `acceptWager` callback in the JavaScript. We'll also define some button handlers in order to attach to the deployed contract.

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L73-L95)

``` js
load: /examples/rps-9-web/index.js
range: 73-95
```

* Line 76: We initialize the component state to display the `Attach` view (figure 12).
* Lines 78 - 82: Defines what happens when the user clicks the `Attach` button.
* Line 79: We call `acc.attach`.
* Line 80: Sets the component state to display the `Attaching` view (figure 13).
* Line 81: Starts running the Reach program as Bob, using the `this` React component as the participant interact interface object.
* Lines 83 - 88: Defines the `acceptWager` callback.
* Lines 85 - 87: Sets the component state to display the `AcceptTerms` view (figure 14) and waits for a `Promise` which can be resolved via user interaction.
* Lines 89 - 92: Defines what happens when the user clicks the `Accept Terms and Pay Wager` button: the `Promise` from line 90 is resolved, and we set the component state to display the `WaitingForTurn` view (figure 15).
* Line 93: Renders the appropriate view from [rps-9-web/views/AttacherViews.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/views/AttacherViews.js).

## Attach Dialog

![Figure 12: The `Attach` view](Attach.png)

## Attaching Display

![Figure 13: The `Attaching` view](Attaching.png)

## Accept Terms Dialog

![Figure 14: The `AcceptTerms` view](AcceptTerms.png)

## Waiting for Turn Display

![Figure 15: The `WaitingForTurn` view](WaitingForTurn.png)

## Putting it All Together

Finally, we call a small helper function from [rps-9-web/views/render.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/views/render.js) to render our App component.

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js#L96)

``` js
load: /examples/rps-9-web/index.js
range: 96
```

As a convenience for running the React development server, you can call:

`$ reach react`

To run the React development server with Algorand, call:

`$ REACH_CONNECTOR_MODE=ALGO reach react`

> # Accessing Algorand Browser Wallet
> If you expect that your Algorand users do not have access to an ARC-0011 browser wallet, you want to provide a fallback.

>If you add the following to your program, then you can provide a simple wallet where the user copies and pastes their mnemonic for each interaction.

>  `reach.setWalletFallback(reach.walletFallback({}));`
> Instead, if you would like to allow your users to use My Algo, then you can add the following:

> ```
  import MyAlgoConnect from '@reach-sh/stdlib/ALGO_MyAlgoConnect';
  stdlib.setWalletFallback(stdlib.walletFallback({
  providerEnv: 'TestNet', MyAlgoConnect }));
  ///
> Of course, you may want to replace
  `'TestNet'`
> with a different network name.

Similarly, to run with Conflux:

`$ REACH_CONNECTOR_MODE=CFX reach react`

> # Adapting for Conflux
> To adapt this example for Conflux TestNet or MainNet, you can add this after the imports:

>  `reach.setProviderByName('TestNet'); // or 'MainNet'`
> For details, see the [Conflux FAQ](https://docs.reach.sh/ref-network-cfx.html#%28part._cfx-faq-mainnet%29).

## Use Reach in Your JavaScript Project

If you'd like to use Reach in your own JavaScript project, you can call:

`npm install @reach-sh/stdlib`

> The Reach standard library is undergoing continual improvement and is updated often. If you are experiencing issues with the Node.js package, try updating!

As usual, you can compile your Reach program `index.rsh` to the backend build artifact `build/index.main.mjs` with:

`$ reach compile`

## Live in the Browser

Now our implementation of _Rock, Paper, Scissors!_ is live in the browser! We can leverage callbacks in the participant interact interface to display to and gather information from the user, through any Web UI framework of our choice.

If we wanted to deploy this application to the world, then we would take the static files that React produces and host them on a web server. These files embed your compiled Reach program, so there's nothing more to do than provide them to the world. 

Next, we'll summarize where we've gone and direct you to the next step of your journey to decentralized application mastery. 

> # Check Your Understanding
> True or false: Reach integrates with all web interface libraries, like React, Vue, and so on, because Reach frontends are just normal JavaScript programs.

> True or false: Reach accelerates your development with React by baking-in a React development server and the deployment process to test React programs locally.

# Onward and Further

Let's review what we've completed in this tutorial:

* In [Scaffolding and Setup](#scaffolding-and-setup) we saw how easy it is to setup initial Reach files and choose a consensus network.
* In [Rock, Paper, and Scissors](#rock-paper-scissors-1) we saw how Reach allows developers to focus on the business logic of their decentralized application and look past the nitty-gritty details of blockchain interaction and protocol design.
* In [Bets and Wagers](#bets-and-wagers) we saw how simple it is for Reach to work with tokens, network transactions and data sharing.
* In [Trust, Commitments, and Attacks](#trust-commitments-and-attacks), we introduced the automatic formal verification engine and how it ensures Reach programs are free of entire categories of flaws and security vulnerabilities.
* In [Timeouts and Participation](#timeouts-and-participation), we learned how Reach protects funds when handling non-participation attacks.
* In [Play and Play Again](#play-and-play-again), we experienced loops in Reach and how flexible the frontend is to variations in the backend.
* In [Interaction and Independence](#interaction-and-independence), we saw how to launch an interactive version of a dApp on a real network.
* In [Web Interaction](#web-interaction-with-react) we deployed our Reach program as a fully decentralized Web app using React.

Despite having done so much, this is only a brief introduction to what is possible with Reach. 

How difficult was it to write a complete dApp using Reach? Let's look at the final versions of our programs to assess.

## Reach Backend

Here's the Reach backend:

[rps-8-interact/index.rsh](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.rsh)

``` js
load: /examples/rps-8-interact/index.rsh
```

## Command-Line

This is the JavaScript command-line frontend:

[rps-8-interact/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/index.mjs)

``` js
load: /examples/rps-8-interact/index.mjs
```

## Web Frontend

Finally, the code of our web frontend:

[rps-9-web/index.js](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-9-web/index.js)

``` js
load: /examples/rps-9-web/index.js
```

## Reach Behind the Scenes

As we've witnessed, we wrote about one-hundred lines of Reach and created two different frontends. Our command-line version is about a hundred lines of JavaScript, while our Web version is about the same length, but it has a lot of presentation code, as well.

Behind the scenes, Reach generated hundreds of lines of Solidity (which you can see here: [rps-8-interact/build/index.main.sol](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/build/index.main.sol)), almost two-thousand lines of TEAL (which is here: [rps-8-interact/build/index.main.appApproval.teal](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/build/index.main.appApproval.teal)), as well as over a thousand lines of JavaScript (as seen here: [rps-8-interact/build/index.main.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-8-interact/build/index.main.mjs)). If we weren't using Reach, we'd have to write all this code ourselves. Not only that, but we’d also need to ensure the frontend and backend were consistent and that they were completely bug and error free.

## Build Your Own!

Now that you've seen an entire Reach application, it's time for you to start working on your own applications!

The very next thing you should do right now is **join us on the [Discord Community](https://discord.gg/AZsgcXu)**! Once you've joined and verified (be sure to have DMs enabled), visit the #tutorial channel and message: "@team, I just completed the tutorial!" and we'll give you the `tutorial veteran` badge, so you can more easily help others work through the tutorials.

If you're interested in using Reach with frontends other than JavaScript, then take time to work through the next section. If you're primarily focused on using Reach with JavaScript, then we encourage you to get active in the Discord (we're excited to welcome you!) and begin building a project. You might also refer to our [JavaScript Frontend](https://reach-sh.github.io/en/books/essentials/frontend-programming/javascript-frontends/) reference material as additional development support.

# Rock, Paper, Scissors in Python
> By Matt Audesse <matt@reach.sh>

Reach supports using any programming language through the [Reach RPC Server](https://docs.reach.sh/ref-backends-rpc.html).

This tutorial walks through an implementation of _Rock, Paper, Scissors!_ using Python in the frontend. This version of the program is based on the [Play and Play Again](https://docs.reach.sh/tut-7.html) section of our _Rock, Paper, Scissors!_ tutorial. While we don't include a text- or web-based interface, we will use the final version of the Reach backend code.

## Comparing JavaScript Frontend to Python

Below, we will compare the _Play and Play Again_ JavaScript frontend with the equivalent Python code communicating via RPC. Follow along by typing the Python code into a file called _index.py_. 

### Imports

Let's begin by comparing the necessary imports and program body:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L1-L5)

``` js
load: /examples/rps-7-loops/index.mjs
range: 1-5
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L1-L10)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 1-10
```

In JavaScript, we import `loadStdlib` and `backend`, but the Python version plucks `mk_rpc` from the supporting `reach_rpc` library. It is unnecessary for an RPC frontend to import a backend because the [RPC server](https://docs.reach.sh/ref-backends-rpc.html) handles this.

The Python version also borrows functionality from the `random` and `threading` libraries. These libraries are critical for the participant interact interface to communicate callable methods to the RPC server.

In line 9 the Python program binds `rpc` and `rpc_callbacks` out of `mk_rpc`. These are the only two functions we need to communicate with the [Python RPC server](https://docs.reach.sh/ref-frontends-rpc-py.html).

### Funding Accounts

Next, we'll define the Alice and Bob accounts and pre-fund them with a balance of 100 network tokens.

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L6-L9)

``` js
load: /examples/rps-7-loops/index.mjs
range: 6-9
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L11-L14)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 11-14
```

Translating code with RPC only requires specifying the corresponding [RPC](https://docs.reach.sh/ref-frontends-rpc-py.html) method and supplying the same arguments. In this snippet, we can see that the Python arguments match the [JavaScript frontend support library](https://docs.reach.sh/ref-frontends-js.html) methods, followed by the parameter seen in the matching line of JavaScript.

### Helper Functions

Next, we'll define two helper functions and use them to query Alice and Bob's beginning balances:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L10-L14)

``` js
load: /examples/rps-7-loops/index.mjs
range: 10-14
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L15-L23)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 15-23
```

### Deploying Contracts

Deploying and attaching contracts works slightly differently over RPC:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L15-L17)

``` js
load: /examples/rps-7-loops/index.mjs
range: 15-17
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L24-L25)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 24-25
```

The RPC Server interfaces with the backend. As such, there is no need to call `backend` as a parameter. Alice's account RPC handle is sufficient for the contract deploy. 

We will delay Bob's contract attach until later because Python lacks promises that are available in JavaScript. When Bob attaches to the account, we'll only need Bob's account RPC handle and Alice's contract [RPC handle](https://docs.reach.sh/ref-backends-rpc-proto.html#%28tech._rpc._handle%29).

## HAND & OUTCOME

`HAND` and `OUTCOME` only differ syntactically from their JavaScript equivalents:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L18-L19)

``` js
load: /examples/rps-7-loops/index.mjs
range: 18-19
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L26-L28)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 26-28
```

## Participant Interact Interface

The participant interact interface definitions remain similar:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L20-L32)

``` js
load: /examples/rps-7-loops/index.mjs
range: 20-32
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L29-L34)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 29-34
```

We see that the JavaScript and Python frontends begin by declaring a reusable Player constructor which provides methods available to the participants.

While the JavaScript includes an explicit call to the `hasRandom` method in the standard library, in Python, we direct the RPC server to append to the interface with `'stdlib.hasRandom' : True` as a field in the constructor's [return value](https://docs.reach.sh/tut-7-rpc.html#py-return). (Seen in line 42, below).

A `getHand` function is defined in line 22, and line 30, respectively. This randomly selects an element from the predefined `HAND` set and returns to the backend. The function will be passed as a callable method of the interface later.

## Timeout

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L36-L38)

``` js
load: /examples/rps-7-loops/index.mjs
range: 36-38
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L35-L37)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 35-37
```

Lines 36 and 35, respectively, show how a participant may be informed of a timeout.

## See Outcome

Similarly, we see the `seeOutcome` function in the following snippet:

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L33-L35)

``` js
load: /examples/rps-7-loops/index.mjs
range: 33-35
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L38-L47)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 38-47
```

In lines 42 - 46 of the Python code, we see a return of `dict`, which represents the most common fields for Alice and Bob's participant interact interface.

In line 42 we see `'stdlib.hasRandom' : True`. When communicating via RPC, this instructs the server to append the signature on the receiving end.

## Build the Game

Finally, we able to use our previous code to play the game!

[rps-7-loops/index.mjs](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-loops/index.mjs#L41-L60)

``` js
load: /examples/rps-7-loops/index.mjs
range: 41-60
```

[rps-7-rpc/client-py/index.py](https://github.com/reach-sh/reach-lang/blob/master/examples/rps-7-rpc/client-py/index.py#L48-L85)

``` python
load: /examples/rps-7-rpc/client-py/index.py
range: 48-85
```

In the Python version, we create a function called `play_alice` and spawn it as a concurrent thread, which begins running in the background with `alice.start()` on line 55.

`play_alice` sends Alice's RPC handle and her participant interact interface to the server with `rpc_callbacks`. The interface includes methods and values created by `player('Alice')` and adds an additional `wager` value which is set to the result of `rpc('/stdlib/parseCurrency', 5), as well as setting a deadline of 10.

Bob's interface is likewise defined and spawned as another thread, which also begins running concurrently on line 69. In Bob's case we add an `acceptWager` method to his participant interact interface. His function is more complex, because we delay creating his contract handle until this time, so that the main thread does not block waiting for Alice's contract information to resolve. This separation is not necessary in JavaScript thanks to JavaScript Promises.

Calling `.join()` in lines 71 & 72 on `alice` and `bob` instructs the main thread to wait until both child threads have completed the game. 

In lines 74 & 75, we collect each player's remaining balance and print them to the console in lines 77 & 78. Each player's child thread will have already printed their success/failure result to the screen prior to reaching this step, because that is how we encoded their `seeOutcome` methods. 

Finally, in lines 80 & 81, we release Alice and Bob's RPC handles from the server's memory with `/forget/acc` and `/forget/ctc`.

Lines 84 & 85 invoke Python's process interpreter 

## Run the Game

Now it's time to play our game!

Move a copy of the `index.rsh` file we used for the [Play and Play Again](https://docs.reach.sh/tut-7.html) section and save it in the same directory as `index.py`.

Next, open a terminal in that directory and install the Reach Python RPC client:

`  $ ([ -d ./venv ] || python3 -m venv ./venv) && source ./venv/bin/activate`

`  $ pip install --upgrade reach-rpc-client`

Then use `reach rpc-run` to play the game of _Rock, Paper, Scissors!_

`  $ reach rpc-run python3 -u ./index.py`

Its output will be the same as the final tutorial version of the frontend:

``` bash
Bob accepts the wager of 5
Alice played Rock
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999
Bob went from 10 to 14.9999
```

This will launch an RPC server using the development [API key](https://docs.reach.sh/ref-backends-rpc-proto.html#%28tech._api._key%29) "opensesame" and a TLS certificate designed for testing.

> Deploying your [DApp](https://docs.reach.sh/ref-model.html#%28tech._dapp%29) into production with the RPC server requires obtaining a certificate which is specific to your DNS domain and which has been signed by a certificate authority such as [Let’s Encrypt](https://letsencrypt.org/getting-started/).
>Users who are ready to go live should consult the [RPC Server command-line](https://docs.reach.sh/ref-usage.html#%28part._ref-usage-rpc-server%29) reference section for configuration details.

Once complete, type `deactivate` to exit the `venv` (virtual environment).

Great work! You've reimplemented the JavaScript tutorial as Python.

## Summary

This tutorial used Python to demonstrate how RPC frontends are built in Reach, but it is similarly easy to write RPC frontends in other languages, such as with the [JavaScript (RPC)](https://docs.reach.sh/ref-frontends-rpc-js.html), [Go (RPC)](https://docs.reach.sh/ref-frontends-rpc-go.html), and [C# RPC](https://docs.reach.sh/ref-frontends-rpc-cs.html) libraries.