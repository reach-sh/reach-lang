


# {#tut} Tutorial

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application and assumes no prior experience with DApp/blockchain development of any kind.
If you want a broad overview before diving in it, we recommend reading [the overview](##overview) first.
On the other hand, if this is too simple, then you may want to start [the workshop](##workshop) for larger and less contrained projects or [the reference manual](##ref) for the minute details of Reach.

If you're ready, click through to the [first step](##tut-1)!

[[toc]]

## {#tut-1} Install and Initialize

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed.
The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop).

::: note
You probably already have `make` installed.
For example, OS X and many other POSIX systems come with `make`, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run `sudo apt install make` to get it.
:::

You'll know that you have everything installed if you can run the following three commands without errors

```
$ make --version
```

```
$ docker --version
```

```
$ docker-compose --version
```


::: note
If you're using Windows, consult [the guide to using Reach on Windows](##guide-windows).
:::

Once you've confirmed that they are installed, choose a directory for this project. We recommend

```
$ mkdir -p ~/reach/tut && cd ~/reach/tut
```


Next, download Reach by running

```
$ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```


You'll know that the download worked if you can run

```
$ ./reach version
```


Since Reach is Dockerized, when you first use it, you'll need to download the images it uses.
This will happen automatically when you first use it, but you can do it manually now by running

```
$ ./reach update
```


You'll know that everything is in order if you can run

```
$ ./reach compile --help
```


---

::: note
Get language support for Reach in your editor by visiting XXX (secref "guide-editor-support").
:::

Now that your Reach installation is in order, you should open a text editor and get ready to [write your first Reach application](##tut-2)!

## {#tut-2} Scaffolding and Setup

In this tutorial, we'll be building a version of _Rock, Paper, Scissors!_ where two players, _Alice_ and _Bob_, can wager on the result of the game.
We'll start simple and slowly make the application more fully-featured.

You should follow along by copying each part of the program and seeing how things go.
If you're like us, you may find it beneficial to type each line out, rather than copying & pasting so you can start building your muscle memory and begin to get a sense for each part of a Reach program.

Let's start by creating a file named `index.rsh`.
It doesn't matter where you put this file, but we recommend putting it in the current directory, which would be `~/reach/tut` if you're following along exactly.
In all the subsequent code samples, we'll label the files based on the chapter of the tutorial you're reading.
For example, start off by typing the following into `index.rsh`:

@[code](@reach-lang/examples/tut-2/index.rsh)

::: note
Did you notice that `export`, `const`, `exit`, and so on are links?
In Reach code samples, you can click on the names of keywords and standard library functions to be brought to their documentation.
:::

::: note
Did you notice that [tut-2/index.rsh](@github/examples/tut-2/index.rsh) was a link in the box above the code sample?
You can always click on these links to see the entire file in our [GitHub](https://github.com/reach-sh/reach-lang) repository.
:::

::: note
Did you notice the attractive clipboard icon on the top the right of that box?
You can click on it and the content of the code box will be copied onto your clipboard.
:::

::: note
Did your text editor recognize `index.rsh` as a Reach program and give you proper syntax hightlighting?
If not, check if there's a plugin available for your editor by visiting XXX (secref "guide-editor-support") or manually
configure it to treat Reach (`.rsh`) files as JavaScript and things will be mostly correct.
:::

This is just a shell of a program that doesn't do much, but it has a few important components.

+ Line 1 indicates that this is a Reach program.
You'll always have this at the top of every program.
+ Line 3 defines the main export from the program.
When you compile, this is what the compiler will look at.
+ Lines 4 through 9 specify the two participants to this application, _Alice_ and _Bob_.
+ Line 10 marks the deployment of the the Reach program, which allows the program to start doing things.


Before we go too much further, let's create a similar shell for our JavaScript frontend code.
Open a new file named `index.mjs` and fill it with this:

@[code](@reach-lang/examples/tut-2/index.mjs)

::: note
Did you notice that `parseCurrency`, `newTestAccount`, `deploy`, and so on are links?
In JavaScript code samples, you can click on the names of standard library functions to be brought to their documentation.
:::

This JavaScript code is similarly schematic and will be consistent across all of your test programs.

+ Line 1 imports the Reach standard library loader.
+ Line 2 imports your backend, which `./reach compile` will produce.
+ Line 3 loads the standard library dynamically based on the `REACH_CONNECTOR_MODE` environment variable.
+ Line 5 defines an asynchronous function that will be the body of our frontend.
+ Line 6 defines a quantity of network tokens as the starting balance for each test account.
+ Lines 7 and 8 create test accounts with initial endowments for Alice and Bob.
This will only work on the Reach-provided developer testing network.
+ Line 10 has Alice deploy the application.
::: note
The program defined in [tut-2/index.rsh](@github/examples/tut-2/index.rsh) will only begin to run after it has been deployed via [tut-2/index.mjs](@github/examples/tut-2/index.mjs).
:::
+ Line 11 has Bob attach to it.
+ Lines 14 through 16 initialize a backend for Alice.
+ Lines 17 through 19 initialize a backend for Bob.
+ Line 13 waits for the backends to complete.
+ Line 20 calls this asynchronous function that we've defined.


This is now enough for Reach to compile and run our program. Let's try by running

```
$ ./reach run
```


Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

::: note
The entire process that we just went through can be automated by running ```
$ ./reach init
```
 when you start your next project!
:::

In [the next step](##tut-3), we'll implement the logic of _Rock, Paper, Scissors!_ and our application will start doing something!

XXX (check:multi
 "2; Reach abstracts away the details of the underlying consensus network"
 "When you write a DApp using Reach, do you"
 "write a smart contract in Solidity, a backend in JavaScript using the Ethereum SDK, and a frontend in JavaScript, then use Reach to test and deploy it;"
 "write a program in Reach that generates a smart contract & a backend and a front-end in JavaScript, then use Reach to test and deploy it?")

## {#tut-3} Rock, Paper, and Scissors

In this section, we'll have Alice and Bob actually execute the game of _Rock, Paper, Scissors!_.

We have to decide how to represent the hands of the game.
A simple way is to represent them as the numbers `0`, `1`, and `2`, standing for `Rock`, `Paper`, and `Scissors`.
However, Reach does not support unsigned integers of exactly two bits, so it is better to represent them as the equivalence class of integers modulo three, so we won't distinguish between `0` and `3` as `Rock`.

We'll use a similar strategy for representing the three outcomes of the game: `B wins`, `Draw`, and `A wins`.

The first step is to change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move that they will play, and later informed of the outcome of the game.

@[code{1-17}](@reach-lang/examples/tut-3/index.rsh)

+ Lines 3 through 6 define a participant interact interface that will be shared between the two players.
In this case, it provides two methods: `getHand`, which returns a number; and `seeOutcome`, which receives a number.
+ Lines 9 through 14 use this interface for both participants.
Because of this line, `interact` in the rest of the program will be bound to an object with methods corresponding to these actions, which will connect to the frontend of the corresponding participant.


Before continuing with the Reach application, let's move over to the JavaScript interface and implement these methods in our frontend.

@[code{13-33}](@reach-lang/examples/tut-3/index.mjs)

+ Lines 13 and 14 define arrays to hold the meaning of the hands and outcomes.
+ Line 15 defines a constructor for the `Player` implementation.
+ Lines 16 through 20 implement the `getHand` method.
+ Lines 21 through 23 implement the `seeOutcome` method.
+ Finally, lines 28 and 31 instantiate the implementation once for Alice and once for Bob.
These are the actual objects that will be bound to `interact` in the Reach program.


There should be nothing interesting or controversial about these implementations; that's the point of Reach: we get to just write normal business logic without worrying about the details of the consensus network and decentralized application.

Let's return to the Reach program and look inside of the body of the program for what actions Alice and Bob take.

In a real-life game of _Rock, Paper, Scissors!_, Alice and Bob simultaneously decide what hand they will play and reveal it at the same time.
"Simultaneity" is a complex concept that is hard to realize in practice.
For example, if you've ever played against a little kid, you may notice them trying to see what you're going to choose and delaying until the last minute to show their hand so they will win.
In a decentralized application, it is not possible to have simultaneity.
Instead, we have to select a particular participant who will "go first".
In this case, we'll choose Alice.

::: note
Does Alice go first, or do we call the player that goes first "Alice"?
This might seem like an unnecessary distinction to make, but it is a very subtle point about the way that Reach works.
In our frontend, we explicitly ran `backend.Alice` and `backend.Bob`.
When we did that, we were committing that particular JavaScript thread to be either Alice or Bob.
In our game, whoever chose to run the Alice backend is the one that will go first.
In other words, **Alice goes first**.
This will be more obvious at [the end of the tutorial](##tut-8) when we'll make the choice interactively about which role to play.
:::

The game proceeds in three steps.

First, the backend for Alice interacts with its frontend, gets Alice's hand, and publishes it.

@[code{17-21}](@reach-lang/examples/tut-3/index.rsh)

+ Line 17 states that this block of code is something that _only_ `Alice` performs.
+ That means that the variable, `handA`, bound on line 13 is known only to Alice.
+ Line 18 binds that value to the result of interacting with Alice through the `getHand` method, which we wrote in JavaScript.
+ Line 18 also declassifies the value, because in Reach, all information from frontends is secret until it is explicitly made public.
+ Line 20 has Alice join the application by publishing the value to the consensus network, so it can be used to evaluate the outcome of the game.
Once this happens, the code is in a "consensus step" where all participants act together.
+ Line 21 commits the state of the consensus network and returns to "local step" where individual participants can act alone.


The next step is similar, in that Bob publishes his hand; however, we don't immediately commit the state, instead we compute the outcome of the game.

@[code{23-29}](@reach-lang/examples/tut-3/index.rsh)

+ Lines 23 through 26 match Alice's similar local step and joining of the application through a consensus transfer publication.
+ But, line 28 computes the outcome of the game before committing.
(`(handA + (4 - handB)) % 3` is a clever equation to compute the winner of a game of _Rock, Paper, Scissors!_ using modular arithmetic.
Consider when `handA` is `0` (i.e., `Rock`) and `handB` is `2` (i.e., `Scissors`),
then this equation is `((handA + (4 - handB)) % 3) = ((0 + (4 - 2)) % 3) = ((0 + 2) % 3) = (2 % 3) = 2`,
which is the last outcome, that is `Alice wins`, as we expect it to be.)


Finally, we use the each form to have each of the participants send the final outcome to their frontends.

@[code{31-33}](@reach-lang/examples/tut-3/index.rsh)

+ Line 31 states that this is a local step that each of the participants performs.


At this point, we can run the program and see its output by running

```
$ ./reach run
```


Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

```
$ ./reach run
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ ./reach run
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ ./reach run
Alice played Paper
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
```


Alice is pretty good at _Rock, Paper, Scissors!_!

Consensus networks in general, and Reach specifically, guarantee that all participants agree on the outcome of their decentralized computation.
Indeed, this is where the name consensus network comes from, as they enable these distributed, and untrusted, parties to come to a consensus, or agreement, about the intermediate states of a computation; and if they agree on the intermediate states, they will also agree on the output.
That's why every time you run `./reach run`, both Alice and Bob will see the same outcome!

::: note
If your version isn't working, look at the complete versions of [tut-3/index.rsh](@github/examples/tut-3/index.rsh) and [tut-3/index.mjs](@github/examples/tut-3/index.mjs) to make sure you copied everything down correctly!
:::

In [the next step](##tut-4), we'll add some stakes to the game, because Alice needs to take her skills to the bank!

XXX (check:many
 (list
  "2 and 3; Reach programs specify a two-way interface between the frontend and the backend via the "
  (tech "participant interact interface")
  ".")
 "Reach programs allow interaction with a user interface through which of the following methods?"
 "by forcing you to write a custom backend for the user interface that connects to the generated smart contract,"
 "by allowing the frontends to provide values directly to the Reach application,"
 "by allowing the Reach program to callback to the frontend via the interact object.")

XXX (check:multi
 (list "2; the " (reachin "publish") " primitive does everything for you.")
 "How do participants in a Reach application share information with each other and find out what others have shared?"
 "Reach generates a smart contract, but you need to implement a process to scan the blockchain for events that correspond to sharing;"
 (item
  "The Reach primitive "
  (reachin "publish")
  " allows a participant to share information with all other participants, which happens automatically without the other parties needing to do anything special;")
 (item
  "The Reach primitive "
  (reachin "publish")
  " allows a participant to share information with all other participants, but they need to explicitly run the receive primitive to receive published information."))

## {#tut-4} Bets and Wagers

Although it's fun to play _Rock, Paper, Scissors!_ with friends for a laugh, it's even better to play it with enemies and your entire life-savings on the line!
Let's change our program so that Alice can offer a wager to Bob and whoever wins will take the pot.

This time, let's start with changes to the JavaScript frontend and then we'll go back into the Reach code and connect the new methods up.

Since we're going to be having funds get transfered, we'll record the balances of each participant before the game starts, so we can more clearly show what they won at the end.
We'll add this code in between account creation and contract deployment.

@[code{6-13}](@reach-lang/examples/tut-4/index.mjs)

+ Line 10 shows a helpful function for displaying currency amounts with up to 4 decimal places.
+ Line 11 shows a helpful function for getting the balance of a participant and displaying it with up to 4 decimal places.
+ Lines 12 and 13 get the balance before the game starts for both Alice and Bob.


Next, we'll update Alice's interface object to include her wager.

@[code{32-35}](@reach-lang/examples/tut-4/index.mjs)

+ Line 33 splices the common `Player` interface into Alice's interface.
+ Line 34 defines her wager as `5` units of the network token.
This is an example of using a concrete value, rather than a function, in a participant interact interface.


For Bob, we'll modify his interface to show the wager and immediately accept it by returning.

@[code{36-41}](@reach-lang/examples/tut-4/index.mjs)

+ Lines 38 through 40 define the `acceptWager` function.


Finally, after the computation is over, we'll get the balance again and show a message summarizing the effect.

@[code{44-48}](@reach-lang/examples/tut-4/index.mjs)

+ Lines 44 and 45 get the balances afterwards.
+ Lines 47 and 48 print out the effect.


These changes to the frontend only deal with issues of presentation and interfacing.
The actual business logic of making the wager and transferring the funds will happen in the Reach code.

Let's look at that now.

First, we need to update the participant interact interface.

@[code{1-19}](@reach-lang/examples/tut-4/index.rsh)

+ Lines 9 through 12 define Alice's interface as the `Player` interface, plus an integer value called `wager`.
+ Lines 13 through 16 do the same for Bob, where he has a method called `acceptWager` that can look at the wager value.


Each of the three parts of the application have to be updated to deal with the wager.
Let's look at Alice's first step first.

@[code{19-25}](@reach-lang/examples/tut-4/index.rsh)

+ Line 20 has Alice declassify the wager for transmission.
+ Line 23 is updated so that Alice shares the wager amount with Bob.
+ Line 24 has her transfer the amount as part of her publication.
The Reach compiler would throw an exception if `wager` did not appear on line 23, but did appear on line 24.
Change the program and try it.
This is because the consensus network needs to be able to verify that the amount of network tokens included in Alice's publication match some computation available to consensus network.


Next, Bob needs to be shown the wager and given the opportunity to accept it and transfer his funds.

@[code{27-32}](@reach-lang/examples/tut-4/index.rsh)

+ Line 28 has Bob accept the wager.
If he doesn't like the terms, his frontend can just not respond to this method and the DApp will stall.
+ Line 32 has Bob pay the wager as well.


The DApp is now running in a consensus step and
the contract itself now holds twice the wager amount.
Before, it would compute the outcome and then commit the state; but now, it needs to look at the outcome and use it to balance the account.

@[code{34-41}](@reach-lang/examples/tut-4/index.rsh)

+ Lines 35 through 38 compute the amounts given to each participant depending on the outcome by determining how many `wager` amounts each party gets.
If the outcome is `2`, `Alice wins`, then she gets two portions; while if it is `0`, `Bob wins`, then he gets two portions; otherwise they each get one portion.
+ Lines 39 and 40 transfer the corresponding amounts.
This transfer takes place from the contract to the participants, not from the participants to each other, because all of the funds reside inside of the contract.
+ Line 41 commits the state of the application and allows the participants to see the outcome and complete.


At this point, we can run the program and see its output by running

```
$ ./reach run
```


Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

```
$ ./reach run
Alice played Paper
Bob accepts the wager of 5.
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.

$ ./reach run
Alice played Paper
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.

$ ./reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.
```


::: note
How come Alice and Bob's balance goes back to `10` each time?
It's because every time we run `./reach run`, it starts a completely fresh instance of the testing network and creates new accounts for each player.
:::

::: note
How come the balances aren't exactly `10`, `15`, and `5`?
It's because Ethereum transactions cost "gas" to run.

If we had shown all the decimals, they'd look like this:

---

```
Alice went from 10 to 14.999999999999687163.
Bob went from 10 to 4.999999999999978229.
...
Alice went from 10 to 4.999999999999687163.
Bob went from 10 to 14.999999999999978246.
```


---

Why does Alice win slightly less than Bob when she wins?
She has to pay to deploy the contract, because she calls `acc.deploy` in her frontend.
The [guide section on deployment](##guide-deploymode) discusses how to avoid this difference.
:::

Alice is doing okay, if she keeps this up, she'll make a fortune on _Rock, Paper, Scissors!_!

::: note
If your version isn't working, look at the complete versions of [tut-4/index.rsh](@github/examples/tut-4/index.rsh) and [tut-4/index.mjs](@github/examples/tut-4/index.mjs) to make sure you copied everything down correctly!
:::

Now that there is a reason to play this game, it turns out that there's a major security vulnerability.
We'll fix this in [the next step](##tut-5); make sure you don't launch with this version, or Alice is going to go broke!

XXX (check:multi
 (list
  "2; the "
  (reachin "pay")
  " and "
  (reachin "transfer")
  " primitives do everything for you.")
 "How do Reach programs manage token funds?"
 "They don’t; you need to manage them explicitly in parallel to the Reach program;"
 (item
  "The "
  (reachin "pay")
  " primitive can be added to a "
  (reachin "publish")
  " primitive to send funds to the Reach program, which can then use the "
  (reachin "transfer")
  " primitive to send funds back to participants, and other addresses."))

## {#tut-5} Trust and Commitments

In the last section, we made it so that Alice and Bob can actually exchange currency when they play _Rock, Paper, Scissors!_.
However, the version of the application we wrote has a fundamental flaw: Bob can win every game!

How is that possible?
We showed executions of the game where Alice won, like the following

```
$ ./reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.
```


The problem is that this version of the game only executed an honest version of Bob, that is, one that followed the Reach program exactly, including in his private local steps.
It is possible for a deviant and dishonest version of a Bob backend to execute different code and always win by computing the appropriate guess based on what value Alice provided for `handA`.

If we change Bob's code to the following:

@[code{27-32}](@reach-lang/examples/tut-5-attack/index.rsh)

then he will ignore the frontend and just compute the correct value.

If we run this version of the program, we will see output like this:

```
$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.
```


In this version, unlike the honest version, Bob never consults the frontend and so it never prints out the message of what hand Bob played.
No matter what Alice chooses, Bob will always win.

---

Is it just a fluke of the random number generator that we observed Bob always winning?
How would we know?
Reach comes with an    [automatic verification](##guide-assert) engine that we can use to mathematically prove that this version will always result in the `outcome` variable equalling `0`, which means Bob wins.
We can instruct Reach to prove this theorem by adding these lines after computing the `outcome`:

@[code{34-37}](@reach-lang/examples/tut-5-attack/index.rsh)

+ Line 35 requires that the dishonest version of Bob be used for the proof.
+ Line 36 conducts the proof by including an assert statement in the program.


Before we had this line in the file, when we ran `./reach run`, it would print out the message:

@[code{2-7}](@reach-lang/examples/tut-4/index.txt)

But now, it prints out

@[code{2-7}](@reach-lang/examples/tut-5-attack/index.txt)

+ Line 7 is different and shows that more theorems have been proven about our program.
It prints out five more, rather than one more, because the theorem is proved differently in the different verification modes.


---

Many programming languages include [assertions](https://en.wikipedia.org/wiki/Assertion_(software_development)) like this,
but Reach is one of a small category where the compiler doesn't just insert a runtime check for the property,
but actually conducts a mathematical proof at compile-time that the expression _always_ evaluates to `true`.

In this case, we used Reach's [automatic verification](##guide-assert) engine to prove that an attack did what we expected it would.
But, it is better to use verification to show that _no flaw_ exists and _no attack_ is possible.

Reach includes some such assertions automatically in every program.
That's why every version of _Rock, Paper, Scissors!_ has said that a number of theorems were checked.
We can see what these theorems do by deliberately inserting an error into the program.

Let's change the computation of the payout and make it so that if Alice wins, then she only gets her wager back, not Bob's.

@[code{34-41}](@reach-lang/examples/tut-5-attack/index-bad.rsh)

+ Line 36 has `[1, 0]`, but should have `[2, 0]`.


When we run `./reach compile (reachexlink tut-5-attack/index-bad.rsh)`, it gives details about the error:

@[code{4-31}](@reach-lang/examples/tut-5-attack/index-bad.txt)

There's a lot of information in the compiler output that can help an experienced programmer track down the problem. But the most important parts are

+ Line 7 says that this is an attempt to prove the theorem that the balance at the end of the program is zero, which means that no network tokens are sealed in the contract forever.
+ Lines 10-20 describe the values that could cause the theorem to fail.
+ Lines 23-31 outline the theorem that failed.


These kinds of [automatic verifications](##guide-assert) are helpful for Reach programmers, because they don't need to remember to put them in their program, and they will still be protected from entire categories of errors.

---

However, now let's add an assertion to the program that will ensure that every version of the program that allows Bob to know Alice's hand before he chooses his own will be rejected.

We'll go back to the version of [tut-4/index.rsh](@github/examples/tut-4/index.rsh) from the last section, which has an honest version of Bob.
(Click on the preceeding link if you need to see what it contained.)

We'll add a single line to the program after Alice publishes, but before Bob takes a local step:

@[code{23-31}](@reach-lang/examples/tut-5-attack/index-fails.rsh)

+ Line 27 contains a knowledge assertion that Bob cannot know Alice's value `handAlice` at this point in the program.
In this case, it is obvious that this is not true, because Alice shares `handAlice` at line 23.
In many cases, this is not obvious and Reach's [automatic verification](##guide-assert) engine has to reason about how values that Bob _does know_ are connected to values that might be related to Alice's secret values.


When we run `./reach run`, it reports that this assertion is false:

@[code{2-6}](@reach-lang/examples/tut-5-attack/index-fails.txt)

It is not enough to correct failures and attacks when you discover them.
You must **always** add an assertion to your program that would fail to hold if the attack or failure were present.
This ensures that all similar attacks are not present and that they will not accidentally be reintroduced.

---

Let's use these insights into [automatic verification](##guide-assert) and rewrite our _Rock, Paper, Scissors!_ so that it is more trustworthy and secure.

Since we've been making lots of changes to the code, let's start fresh with a new version and we'll look at every single line again, to make sure that you aren't missing anything.

First, we'll define the rules of _Rock, Paper, Scissors!_ a little bit more abstractly, so we can separate the logic of the game from the details of the application:

@[code{1-7}](@reach-lang/examples/tut-5/index.rsh)

+ Line 1 is the usual Reach version header.
+ Lines 3 and 4 define enumerations for the hands that may be played, as well as the outcomes of the game.
+ Lines 6 and 7 define the function that computes the winner of the game.


When we first wrote _Rock, Paper, Scissors!_, we asked you to trust that this formula for computing the winner is correct, but it is good to actually check.
One way to check would be to implement a JavaScript frontend that didn't interact with a real user, nor would it randomly generate values, but instead, it would return specific testing scenario values and check that the output is as expected.
That's a typical way to debug and is possible with Reach.
However, Reach allows us to write such test cases directly into the Reach program as verification assertions.

@[code{9-11}](@reach-lang/examples/tut-5/index.rsh)

+ Line 9 makes an assertion that when Alice plays Rock and Bob plays Paper, then Bob wins as expected.


But, Reach's [automatic verification](##guide-assert) allows us to express even more powerful statements about our program's behavior.
For example, we can state that no matter what values are provided for `handA` and `handB`, `winner` will always provide a valid outcome:

@[code{13-15}](@reach-lang/examples/tut-5/index.rsh)

And we can specify that whenever the same value is provided for both hands, no matter what it is, `winner` always returns `DRAW`:

@[code{17-18}](@reach-lang/examples/tut-5/index.rsh)

These examples both use `forall`, which allows Reach programmers to quantify over all possible values that might be provided to a part of their program.
You might think that these theorems will take a very long time to prove, because they have to loop over all the billions and billions of possibilities (e.g., Ethereum uses 256-bits for its unsigned integers) for the bits of `handA` (twice!) and `handB`.
In fact, on the author's MacBook Pro from early 2015, it takes less than half a second.
That's because Reach uses an advanced [symbolic execution engine](##guide-reach) to reason about this theorem abstractly without considering individual values.

Let's continue the program by specifying the participant interact interfaces for Alice and Bob.
These will be mostly the same as before, except that we will also expect that each frontend can provide access to random numbers.
We'll use these later on to protect Alice's hand.

@[code{20-24}](@reach-lang/examples/tut-5/index.rsh)

The only line that is different is line 21, which includes `hasRandom`, from the Reach standard library, in the interface.

@[code{20-30}](@reach-lang/examples/tut-5/index.mjs)

Similarly, we only need to modify one line of our JavaScript frontend.
Line 21 allows each participant's Reach code to generate random numbers as necessary.

These two changes might look identical, but they mean very different things.
The first, line 21 in the Reach program, adds `hasRandom` to the interface that the backend expects the frontend to provide.
The second, line 21 in the JavaScript, adds `hasRandom` to the implementation that the frontend provides to the backend.

We're now at the crucial juncture where we will implement the actual application and ensure that Alice's hand is protected until after Bob reveals his hand.
The simplest thing would be to have Alice just publish the wager, but this, of course, would just leave Bob vulnerable.
We need Alice to be able to publish her hand, but also keep it secret.
This is a job for a [cryptographic commitment scheme](https://en.wikipedia.org/wiki/Commitment_scheme).
Reach's standard library comes with `makeCommitment` to make this easier for you.

@[code{37-45}](@reach-lang/examples/tut-5/index.rsh)

+ Line 39 has Alice compute her hand, but _not_ declassify it.
+ Line 40 has her compute a commitment to the hand.
It comes with a secret "salt" value that must be revealed later.
+ Line 41 has Alice declassify the commitment.
+ Line 43 has her publish them, and line 44 has her include the wager funds in the publication.


At this point, we can state the knowledge assertion that Bob can't know either the hand or the "salt" and continue with his part of the program.

::: note
It is important to include the salt in the commitment, so that multiple commitments to the same value are not identical.
Similarly, it is important not to share the salt until later, because if an attacker knows the set of possible values, they can enumerate them and compare with the result of the commitment and learn the value.
:::

@[code{47-54}](@reach-lang/examples/tut-5/index.rsh)

+ Line 47 states the knowledge assertion.
+ Lines 48 through 53 are unchanged from the original version.
+ Line 54 has the transaction commit, without computing the payout, because we can't yet, because Alice's hand is not yet public.


We now return to Alice who can reveal her secrets.

@[code{56-61}](@reach-lang/examples/tut-5/index.rsh)

+ Lines 57 and 58 have Alice declassify the secret information.
+ Line 60 has her publish it.
+ Line 61 checks that the published values match the original values.
This will always be the case with honest participants, but dishonest participants may violate this assumption.


The rest of the program is unchanged from the original version, except that it uses the new names for the outcomes:

@[code{63-74}](@reach-lang/examples/tut-5/index.rsh)

Since we didn't have to change the frontend in any meaningful way, the output of running `./reach run` is still the same as it ever was:

```
$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.
Bob played Paper
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.

$ ./reach run
Alice played Paper
Bob accepts the wager of 5.
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.

$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.
Bob played Scissors
Bob saw outcome Draw
Alice saw outcome Draw
Alice went from 10 to 9.9999.
Bob went from 10 to 9.9999.
```


Except now, behind the scenes, and without any changes to the frontend, Alice takes two steps in our program and Bob only takes one, and she is protected against Bob finding her hand and using it to ensure he wins!

When we compile this version of the application, Reach's [automatic formal verification](##guide-assert) engine proves many theorems and protects us against a plethora of mistakes one might make when writing even a simple application like this.
Non-Reach programmers that try to write decentralized applications are on their own trying to ensure that these problems don't exist.

::: note
If your version isn't working, look at the complete versions of [tut-5/index.rsh](@github/examples/tut-5/index.rsh) and [tut-5/index.mjs](@github/examples/tut-5/index.mjs) to make sure you copied everything down correctly!
:::

Now our implementation of _Rock, Paper, Scissors!_ is secure and doesn't contain any exploits for either Alice or Bob to guarantee a win.
However, it still has a final category of mistake that is common in decentralized applications: [non-participation](##guide-timeout).
We'll fix this in [the next step](##tut-6); make sure you don't launch with this version, or Alice may decide to back out of the game when she knows she's going to lose!

XXX (check:tf
 "False"
 "Since blockchain programs run on a single, global, publicly-checked and certified consensus network, you don’t need to test them as much as normal software, which run on a wide variety of different platforms and operating systems.")

XXX (check:tf
 "False"
 "It is easy to write correct programs that handle financial information, and even if you make a mistake, blockchains support an \"Undo\" operation that allows you to rollback to earlier versions of the ledger to correct mistakes and recover lost funds.")

XXX (check:tf
 "True"
 "Reach provides automatic verifications to ensure that your program does not lose, lock away, or overspend funds and guarantees that your applications are free from entire categories of errors.")

XXX (check:tf
 "True"
 "Reach provides tools for you to add custom verifications to your program, like ensuring that information is known only to one party, or that your implementation of a sensitive algorithm is correct.")

## {#tut-6} Timeouts and Participation

In the last section, we removed a security vulnerability from _Rock, Paper, Scissors!_ that was a clear attack on the viability of the application.
In this section, we'll focus on a more subtle issue that is important and unique to decentralized applications: [non-participation](##guide-timeout).

Non-participation refers to the act of one party ceasing to continue playing their role in an application.

In traditional client-server programs, like a Web server, this would be the case of a client not sending any more requests to the server, or the server stopping sending responses to the client.
In these sorts of traditional programs, non-participation is an exceptional circumstance that normally leads to an error message for clients and, at most, a log entry for servers.
Sometimes traditional programs will need to recycle resources, like network ports, on non-participation, but they would have also needed to do that if the transaction ended by normal means.
In other words, for traditional client-server programs, it is not necessary for designers to meticulously consider the consequences of non-participation.

In contrast, decentralized applications must be carefully designed with an eye towards their behavior in the face of non-participation.
For example, consider what happens in our _Rock, Paper, Scissors!_ game if after Alice has paid her wager, Bob never accepts and the application doesn't continue.
In this case, Alice's network tokens would be locked inside of the contract and lost to her.
Similarly, if after Bob accepted and paid his wager, Alice stopped participating and never submitted her hand, then both their funds would be locked away forever.
In each of these cases, both parties would be greatly hurt and their fear of that outcome would introduce an additional cost to transacting, which would lower the value they got from participating in the application.
Of course, in a situation like _Rock, Paper, Scissors!_ this is unlikely to be an important matter, but recall that _Rock, Paper, Scissors!_ is a microcosm of decentralized application design.

::: note
Technically, in the first case, when Bob fails to start the application, Alice is not locked away from her funds: since Bob's identity is not fixed until after his first message, she could start another instance of the game as the Bob role and then she'd win all of the funds, less any transaction costs of the consensus network.
In the second case, however, there would be no recourse for either party.
:::

In the rest of this section, we'll discuss how Reach helps address non-participation.
For a longer discussion, refer to [the guide chapter on non-participation](##guide-timeout).

---

In Reach, non-participation is handled through a "timeout" mechanism whereby each consensus transfer can be paired with a step that occurs for all participants if the originator of the consensus transfer fails to make the required publication before a particular network time.
We'll integrate this mechanism into our version of _Rock, Paper, Scissors!_ and deliberately insert non-participation into our JavaScript testing program to watch the consequences play out.

First, we'll modify the participant interact interface to allow the frontend to be informed that a timeout occurred.

@[code{20-25}](@reach-lang/examples/tut-6/index.rsh)

+ Line 24 introduces a new method, `informTimeout`, that receives no arguments and returns no information.
We'll call this function when a timeout occurs.


We'll make a slight tweak to our JavaScript frontend to be able to receive this message and display it on the console.

@[code{20-33}](@reach-lang/examples/tut-6/index.mjs)

Back in the Reach program, we'll declare a value to use as a standard deadline throughout the program.
Similar to how she provides the wager, we will have Alice also provide the deadline.

@[code{28-32}](@reach-lang/examples/tut-6/index.rsh)

+ Line 31 adds the `deadline` field to Alice's participant interact interface.
It is defined as some number of time delta units,
which are an abstraction of the underlying notion of network time in the consensus network.
In many networks, like Ethereum, this number is a number of blocks.


Next, at the start of the Reach application, we'll define a helper function to inform each of the participants of the timeout by calling this new method.

@[code{39-43}](@reach-lang/examples/tut-6/index.rsh)

+ Line 39 defines the function as an arrow expression.
+ Line 40 has each of the participants perform a local step.
+ Line 41 has them call the new `informTimeout` method.


We will have Alice declassify and publish the `deadline` for later timeout clauses.

We won't add a timeout clause to Alice's first message, because there is no consequence to her non-participation:
if she doesn't start the game, then no one is any worse off.

@[code{45-54}](@reach-lang/examples/tut-6/index.rsh)

+ Line 50 has Alice declassify the `deadline` time delta.
+ Line 51 now has Alice publish the `deadline`.


However, we will adjust Bob's first message, because if he fails to participate, then Alice's initial wager will be lost to her.

@[code{61-64}](@reach-lang/examples/tut-6/index.rsh)

+ Line 63 adds a timeout handler to Bob's publication.


The timeout handler specifies that if Bob does not complete this action within a time delta of `deadline`, then the application transitions to step given by the arrow expression.
In this case, the timeout code is a call to `closeTo`, which is a Reach standard library function that has Alice send a message and transfer all of the funds in the contract to herself, then call the given function afterwards.
This means that if Bob fails to publish his hand, then Alice will take her network tokens back.

We will add a similar timeout handler to Alice's second message.

@[code{70-71}](@reach-lang/examples/tut-6/index.rsh)

But in this case, Bob will be able to claim all of the funds if Alice doesn't participate.
You might think that it would be "fair" for Alice's funds to be returned to Alice and Bob's to Bob.
However, if we implemented it that way,
then Alice would be wise to always timeout if she were going to lose,
which she knows will happen, because she knows her hand and Bob's hand.

These are the only changes we need to make to the Reach program to make it robust against non-participation: eleven lines!

---

Let's modify the JavaScript frontend to deliberately cause a timeout sometimes when Bob is supposed to accept the wager.

@[code{35-54}](@reach-lang/examples/tut-6/index.mjs)

+ Line 39 has Alice specify a `deadline` of ten blocks.
+ Lines 43 through 52 redefine Bob's `acceptWager` method as an asynchronous function,
where half of the time it will take at least ten blocks on the Ethereum network by waiting for ten units of time to pass.
We know that ten is the value of `deadline`, so this will cause a timeout.


---

Let's run the program and see what happens:

```
$ ./reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.

$ ./reach run
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

$ ./reach run
Alice played Paper
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


Of course, when you run, you may not get two of the three times ending in a timeout.

::: note
If your version isn't working, look at the complete versions of [tut-6/index.rsh](@github/examples/tut-6/index.rsh) and [tut-6/index.mjs](@github/examples/tut-6/index.mjs) to make sure you copied everything down correctly!
:::

Now our implementation of _Rock, Paper, Scissors!_ is robust against either participant dropping from the game.
In [the next step](##tut-7), we'll extend the application to disallow draws and have Alice and Bob play again until there is a winner.

XXX (check:multi
 "4; Reach empowers programmers to design the application with the business logic they want."
 "What happens in a decentralized application when one participant refuses to take the next step of the program? For example, if Alice refuses to share her hand with Bob in a game of ‘Rock, Paper, Scissors’."
 "This is not possible, because the blockchain guarantees that each party performs a particular set of actions;"
 "The program hangs forever waiting for Alice to provide the value;"
 "Alice is punished and the program proceeds as-if Bob were the winner;"
 (item
  "It depends on how the program was written; if the developer used Reach, the default is (2), but the developer could include a "
  (reachin "timeout")
  " block to implement the (3) behavior."))

## {#tut-7} Play and Play Again

In this section, we extend our application so that Alice and Bob will continue to play against each other until there is a clear winner, so if it is a draw they will continue playing.

This will only require a change to the Reach program, not the JavaScript frontend, but we will take the opportunity to modify the frontend so that timeouts can happen to both parties when they are asked to submit their hands.
Let's do that to get it out of the way and not distract from the main task of removing draws.

We'll modify the `Player` interact object so that it will have a different `getHand` method.

@[code{20-39}](@reach-lang/examples/tut-7/index.mjs)

+ Lines 25 through 30 moves the forced timeout code that we wrote for Bob's `acceptWager` function into this method.
We also change the threshold so that timeouts only happen 1% of the time.
This isn't a very interesting behavior, so we'll make it much less frequent.


We also adjust Bob's `acceptWager` function to remove the timeout code, since we're testing that differently now.
It's just a matter of reverting to the simpler version from before.

@[code{41-53}](@reach-lang/examples/tut-7/index.mjs)

+ Lines 49 through 51 have the simpler `acceptWager` method for Bob.


---

Now, let's look at the Reach application.
All of the details about the playing of the game and the interface to the players will remain the same.
The only thing that's going to be different is the order the actions take place.

It used to be that the steps were:

1. Alice sends her wager and commitment.
2. Bob accepts the wager and sends his hand.
3. Alice reveals her hand.
4. The game ends.


But, now because the players may submit many hands, but should only  have a single wager, we'll break these steps up differently, as follows:

1. Alice sends her wager.
2. Bob accepts the wager.
3. Alice sends her commitment.
4. Bob sends his hand.
5. Alice reveals her hand.
6. If it's draw, return to step 3; otherwise, the game ends.


Let's make these changes now.

@[code{45-51}](@reach-lang/examples/tut-7/index.rsh)

+ Line 49 has Alice publish the wager and deadline.
+ Line 50 has Alice pay the wager.


@[code{53-58}](@reach-lang/examples/tut-7/index.rsh)

+ Line 56 has Bob pay the wager.
+ Line 58 does **not** have this consensus step commit.


---

It's now time to begin the repeatable section of the application, where each party will repeatedly submit hands until the the outcome is not a draw.
In normal programming languages, such a circumstance would be implemented with a `while` loop, which is exactly what we'll do in Reach.
However, `while` loops in Reach require extra care, as discussed in [the guide on loops in Reach](##guide-loop-invs), so we'll take it slow.

In the rest of a Reach program, all identifier bindings are static and unchangable, but if this were the case throughout all of Reach, then `while` loops would either never start or never terminate, because the loop condition would never change.
So, a `while` loop in Reach can introduce a variable binding.

Next, because of Reach's [automatic verification](##guide-assert) engine, we must be able to make a statement about what properties of the program are invariant before and after a `while` loop body's execution, a so-called ["loop invariant"](##guide-loop-invs).

Finally, such loops _may only occur_ inside of consensus steps.
That's why Bob's transaction was not committed, because we need to remain inside of the consensus to start the `while` loop.
This is because all of the participants must agree on the direction of control flow in the application.

Here's what the structure looks like:

@[code{59-61}](@reach-lang/examples/tut-7/index.rsh)

+ Line 59 defines the loop variable, `outcome`.
+ Line 60 states the invariant that the body of the loop does not change the balance in the contract account and that  `outcome` is a valid outcome.
+ Line 61 begins the loop with the condition that it continues as long as the outcome is a draw.


Now, let's look at the body of the loop for the remaining steps, starting with Alice's commitment to her hand.

@[code{62-71}](@reach-lang/examples/tut-7/index.rsh)

+ Line 62 commits the last transaction, which at the start of the loop is Bob's acceptance of the wager, and at subsequent runs of the loop is Alice's publication of her hand.
+ Lines 64 through 71 are almost identical to the older version, except the wager is already known and paid.


@[code{73-79}](@reach-lang/examples/tut-7/index.rsh)

Similarly, Bob's code is almost identical to the prior version, except that he's already accepted and paid the wager.

@[code{81-87}](@reach-lang/examples/tut-7/index.rsh)

Alice's next step is actually identical, because she is still revealing her hand in exactly the same way.

Next is the end of the loop.

@[code{89-91}](@reach-lang/examples/tut-7/index.rsh)

+ Line 89 updates the `outcome` loop variable with the new value.
+ Line 90 continues the loop.
Unlike most programming languages, Reach **requires** that `continue` be explicitly written in the loop body.


The rest of the program could be exactly the same as it was before, except now it occurs outside of the loop, but we will simplify it, because we know that the outcome can never be a draw.

@[code{93-95}](@reach-lang/examples/tut-7/index.rsh)

+ Line 93 asserts that the outcome is never draw, which is trivially true because otherwise the `while` loop would not have exitted.
+ Line 94 transfers the funds to the winner.


---

Let's run the program and see what happens:

```
$ ./reach run
Bob accepts the wager of 5.
Alice played Paper
Bob played Rock
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.

$ ./reach run
Bob accepts the wager of 5.
Alice played Rock
Bob played Rock
Alice played Paper
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.

$ ./reach run
Bob accepts the wager of 5.
Alice played Scissors
Bob played Rock
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.
```


As usual, your results may differ, but you should be able to see single round victories like this, as well as multi-round fights and timeouts from either party.

::: note
If your version isn't working, look at the complete versions of [tut-7/index.rsh](@github/examples/tut-7/index.rsh) and [tut-7/index.mjs](@github/examples/tut-7/index.mjs) to make sure you copied everything down correctly!
:::

Now our implementation of _Rock, Paper, Scissors!_ will always result in a pay-out, which is much more fun for everyone.
In [the next step](##tut-8), we'll show how to exit "testing" mode with Reach and turn our JavaScript into an interactive _Rock, Paper, Scissors!_ game with real users.

XXX (check:multi
 (list "2; Reach supports " (reachin "while") " loops.")
 "How do you write an application in Reach that runs arbitrarily long, like a game of Rock, Paper, Scissors that is guaranteed to not end in a draw?"
 "This is not possible, because all Reach programs are finitely long;"
 (item
  "You can use a "
  (reachin "while")
  " loop that runs until the outcome of the game is decided."))

XXX (check:many
 (list "All of the above.")
 (list
  "When you check if a program with a "
  (reachin "while")
  " loop is correct, you need to have a property called a loop invariant. Which of the following statements have to be true about the loop invariant?")
 (item
  "The part of the program before the "
  (reachin "while")
  " loop must establish the invariant.")
 "The condition and the body of the loop must establish the invariant."
 "The negation of the condition and the invariant must establish any properties of the rest of the program.")

## {#tut-8} Interaction and Independence

In the last section, we made our _Rock, Paper, Scissors!_ run until there was a definitive winner.
In this section, we won't be making any changes to the Reach program itself.
Instead, we'll go under the covers of `reach run`, as well as build a version of our game that is interactive and can be played away from a private developer test network.

---

In the past, when we've run `./reach run`, it would create a Docker image just for our Reach program that contained a temporary Node.js package connecting our JavaScript frontend to the Reach standard library and a fresh instance of a private developer test network.
In this section we'll introduce customizations in support of a non-automated version of _Rock, Paper, Scissors!_ and provide the option to connect to a real Ethereum network.

We'll start by running

```
$ ./reach scaffold
```


which will automatically generate the following files for us:

+ `package.json` --- A Node.js package file that connects our `index.mjs` to the Reach standard library.
+ `Dockerfile` --- A Docker image script that builds our package efficiently and runs it.
+ `docker-compose.yml` --- A Docker Compose script that connects our Docker image to a fresh instance of the Reach private developer test network.
+ `Makefile` --- A `Makefile` that easily rebuilds and runs the Docker image.


We're going to leave the first two files unchanged.
You can look at them at [tut-8/package.json](@github/examples/tut-8/package.json) and [tut-8/Dockerfile](@github/examples/tut-8/Dockerfile), but the details aren't especially important.
However, we'll customize the other two files.

First, let's look at the [tut-8/docker-compose.yml](@github/examples/tut-8/docker-compose.yml) file:

@[code](@reach-lang/examples/tut-8/docker-compose.yml)

+ Lines 2 and 3 define a service for starting our application.
Your line 3 will say `tut`, rather than `tut-8`, if you've stayed in the same directory throughout the tutorial.
+ Lines 5 and 6 define the Reach private developer test network service for Conflux.
+ Lines 7 and 8 define the Reach private developer test network service for Ethereum.
+ Lines 9 through 26 define the Reach private developer test network service for Algorand.
+ Lines 27 through 82 define services that allow the application to be run with different networks; including line 27, which defines `reach-app-tut-8-ETH-live` for connecting to a live network.
+ We'll also add lines 85 through 90 to define a `player` service that is our application with an open standard input, as well as two instances named `alice` and `bob`.


With these in place, we can run

```
$ docker-compose run WHICH
```


where `WHICH` is `reach-app-tut-8-ETH-live` for a live instance, or `alice` or `bob` for a test instance.
If we use the live version, then we have to define the environment variable `ETH_NODE_URI` as the URI of our Ethereum node.

We'll modify the [tut-8/Makefile](@github/examples/tut-8/Makefile) to have commands to run each of these variants:

@[code{37-44}](@reach-lang/examples/tut-8/Makefile)

However, if we try to run either of these, it will do the same thing it always has: create test accounts for each user and simulate a random game.
Let's modify the JavaScript frontend and make them interactive.

---

We'll start from scratch and show every line of the program again.
You'll see a lot of similarity between this and the last version, but for completeness, we'll show every line.

@[code{1-6}](@reach-lang/examples/tut-8/index.mjs)

+ Lines 1, 2, and 4 are the same as before: importing the standard library and the backend.
+ Line 3 is new and imports a helpful library for simple console applications called `ask.mjs` from the Reach standard library.
We'll see how these three functions are used below.


@[code{7-12}](@reach-lang/examples/tut-8/index.mjs)

+ Lines 7 through 10 ask the question whether they are playing as Alice and expect a "Yes" or "No" answer.
`ask` presents a prompt and collects a line of input until its argument does not error.
`yesno` errors if it is not given "y" or "n".


@[code{13-29}](@reach-lang/examples/tut-8/index.mjs)

+ Lines 16 through 19 present the user with the choice of creating a test account if they can or inputting a secret to load an existing account.
+ Line 21 creates the test account as before.
+ Line 27 loads the existing account.


@[code{30-46}](@reach-lang/examples/tut-8/index.mjs)

+ Lines 31 through 34 ask if the participant will deploy the contract.
+ Lines 36 through 38 deploy it and print out public information (`ctc.getInfo`) that can be given to the other player.
+ Lines 40 through 44 request, parse, and process this information.


@[code{47-54}](@reach-lang/examples/tut-8/index.mjs)

Next we define a few helper functions and start the participant interaction interface.

@[code{55-59}](@reach-lang/examples/tut-8/index.mjs)

First we define a timeout handler.

@[code{60-78}](@reach-lang/examples/tut-8/index.mjs)

Next, we request the wager amount or define the `acceptWager` method, depending on if we are Alice or not.

@[code{79-97}](@reach-lang/examples/tut-8/index.mjs)

Next, we define the shared `getHand` method.

@[code{98-102}](@reach-lang/examples/tut-8/index.mjs)

Finally, the `seeOutcome` method.

@[code{103-111}](@reach-lang/examples/tut-8/index.mjs)

Lastly, we choose the appropriate backend function and await its completion.

---

We can now run

```
$ make build
```


to rebuild the images, then

```
$ make run-alice
```


in one terminal in this directory and

```
$ make run-bob
```


in another terminal in this directory.

Here's an example run:

```
$ make run-alice
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


and

```
$ make run-bob
Are you Alice?
n
Starting Rock, Paper, Scissors as Bob
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the contract? (y/n)
n
Please paste the contract information:
"0xc2a875afbdFb39b1341029A7deceC03750519Db6"
Your balance is 1000
Do you accept the wager of 10?
y
What hand will you play?
p
You played Paper
The outcome is: Bob wins
Your balance is now 1009.9999
```


Of course, when you run the exact amounts and addresses may be different.

::: note
If your version isn't working, look at the complete versions of [tut-8/index.rsh](@github/examples/tut-8/index.rsh), [tut-8/index.mjs](@github/examples/tut-8/index.mjs), [tut-8/package.json](@github/examples/tut-8/package.json), [tut-8/Dockerfile](@github/examples/tut-8/Dockerfile), [tut-8/docker-compose.yml](@github/examples/tut-8/docker-compose.yml), and [tut-8/Makefile](@github/examples/tut-8/Makefile) to make sure you copied everything down correctly!
:::

---

If we were to edit [tut-8/docker-compose.yml](@github/examples/tut-8/docker-compose.yml), and move the `&default-app` on line 34 to line 54, then instead of running on Ethereum, we'd be able to test and run our application on Algorand.

---

Now our implementation of _Rock, Paper, Scissors!_ is finished!
We are protected against attacks, timeouts, and draws, and we can run interactively on non-test networks.

In this step, we made a command-line interface for our Reach program.
In [the next step](##tut-9), we'll replace this with a Web interface for the same Reach program.

XXX (check:tf
 "False; Reach does not impose any constraints on what kind of frontend is attached to your Reach application."
 "Reach helps you build automated tests for your decentralized application, but it doesn’t support building interactive user-interfaces.")

## {#tut-9} Web Interaction


In the last section, we made _Rock, Paper, Scissors!_ run as a command-line application, without any changes to the Reach program.
In this section, we again won't be making any changes to the Reach program.
Instead, we'll replace the command-line interface with a Web interface.

We will use [React.js](https://reactjs.org/) for this tutorial, but the same principles apply to any Web framework.

::: note
If you've never used React before, here are some basics about how it works:
+ React programs are JavaScript programs that use a special library that allows you to mix HTML inside of the body of your JavaScript.
+ React has a special compiler that combines a bundle of JavaScript programs, and all their dependencies, into one large file that can be deployed on a static Web server.
This is called "packing".
+ When you're developing and testing with React, you run a special development Web server that watches and updates this packed file every time you modify a source file, so you don't have to constantly run the compiler.
+ Reach automates the process of starting this development server for you when you run `./reach react` and gives you access to it at `http://localhost:3000/`.

:::

Similarly, in this tutorial, we assume that we will be deploying (and testing) with Ethereum.
Reach Web applications rely on the Web browser to provide access to a consensus network account and its associated wallet.
On Ethereum, the standard wallet is [MetaMask](https://metamask.io).
If you want to test this code, you'll need to install it and set it up.
Furthermore, MetaMask does not support multiple active accounts, so if you want to test _Rock, Paper, Scissors!_ locally, you'll need to have two separate browser instances: one to act as Alice and another to act as Bob.

---

The code in this section does not use the scaffolding from the previous section.
Reach comes with a convenience command for deleting scaffolded files:

```
$ ./reach unscaffold
```


Similarly, you do not need the previous `index.mjs` file, because we'll be writing it completely from scratch to use React.
You can run the following command to delete it:

```
$ rm index.mjs
```


Or, you can copy the `index.rsh` file into a new directory and work from there.

---

This code is supplemented with [index.css](@github/examples/tut-9/index.css)
and some [views](@github/examples/tut-9/views).
These details are not specific to Reach, and are fairly trivial,
so we will not explain the specifics of those files.
If you run this locally, you'll want to download those files.
Your directory should look like:

```
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


---

We will focus on [tut-9/index.js](@github/examples/tut-9/index.js),
because [tut-9/index.rsh](@github/examples/tut-9/index.rsh) is the same as previous sections.

@[code{1-9}](@reach-lang/examples/tut-9/index.js)

On lines 1 thru 6, we import our view code and CSS.
On line 7, we import the compiled `backend`.
On lines 8 and 9, we load the `stdlib` as `reach`.

React compiles the Reach standard libray in such a way that
it does not have direct access to the environment variables
which are used to select the desired standard library.
This is why you need to pass `process.env` as an argument
to achieve the desired effect.

@[code{10-14}](@reach-lang/examples/tut-9/index.js)

On these lines we define a few helpful constants and defaults for later,
some corresponding to the enumerations we defined in Reach.

--- 
We start defining `App` as a React component,
and tell it what to do once it mounts, which is the React term for starting.

XXX (exviewfigs "tut-9" "AppViews" '("ConnectAccount" 19 28))

@[code{15-31}](@reach-lang/examples/tut-9/index.js)

@[code{39-41}](@reach-lang/examples/tut-9/index.js)

+ On line 18, we initialize the component state to display XXX (exviewref "tut-9" "ConnectAccount").
+ On lines 20 thru 31, we hook into React's `componentDidMount` lifecycle event, which is called when the component starts.
+ On line 21, we use `getDefaultAccount`, which accesses the default browser account.
For example, when used with Ethereum, it can discover the currently-selected MetaMask account.
+ On line 26, we use `getFaucet` to try and access the Reach developer testing network faucet.
+ On line 27, if `getFaucet` was successful, we set the component state to display XXX (exviewref "tut-9" "FundAccount").
+ On line 29, if `getFaucet` was unsuccessful, we set the component state to skip to XXX (exviewref "tut-9" "DeployerOrAttacher").
+ On line 39, we render the appropriate view from [tut-9/views/AppViews.js](@github/examples/tut-9/views/AppViews.js).


XXX (exviewfigs "tut-9" "AppViews" '("FundAccount" 30 54))

Next, we define callbacks on `App` for what to do when the user clicks certain buttons.

@[code{32-36}](@reach-lang/examples/tut-9/index.js)

+ On lines 32 thru 35, we define what happens when the user clicks the `Fund Account` button.
+ On line 33, we transfer funds from the faucet to the user's account.
+ On line 34, we set the component state to display XXX (exviewref "tut-9" "DeployerOrAttacher").
+ On line 36, we define what to do when the user clicks the `Skip` button,
which is to set the component state to display XXX (exviewref "tut-9" "DeployerOrAttacher").


XXX (exviewfigs "tut-9" "AppViews" '("DeployerOrAttacher" 56 78))

@[code{37-38}](@reach-lang/examples/tut-9/index.js)

On lines 37 and 38, we set a sub-component
based on whether the user clicks `Deployer` or `Attacher`.

--- 
Next, we will define `Player` as a React component,
which will be extended by the specialized components for Alice and Bob.

XXX (exviewfigs "tut-9" "PlayerViews" '("GetHand" 8 32))

Our Web frontend needs to implement the participant interact interface for players, which we defined as:

@[code{20-25}](@reach-lang/examples/tut-9/index.rsh)

We will provide these callbacks via the React component directly.

@[code{42-55}](@reach-lang/examples/tut-9/index.js)

+ On line 43, we provide the `random` callback
+ On lines 44 thru 50, we provide the `getHand` callback.
+ On lines 45 thru 47, we set the component state to display XXX (exviewref "tut-9" "GetHand"),
and wait for a `Promise` which can be resolved via user interaction.
+ On line 48, which occurs after the `Promise` is resolved,
we set the component state to display XXX (exviewref "tut-9" "WaitingForResults").
+ On lines 51 and 52, we provide the `seeOutcome` and `informTimeout` callbacks,
which set the component state to display XXX (exviewref "tut-9" "Done") and XXX (exviewref "tut-9" "Timeout"), respectively.
+ On line 53, we define what happens when the user clicks `Rock`, `Paper`, or `Scissors`:
The `Promise` from line 45 is resolved.


XXX (exviewfigs
 "tut-9"
 "PlayerViews"
 '("WaitingForResults" 34 42)
 '("Done" 44 54)
 '("Timeout" 56 64))

--- 
Next, we will define `Deployer` as a React component for Alice,
which extends `Player`.

XXX (exviewfigs "tut-9" "DeployerViews" '("SetWager" 20 38) '("Deploy" 40 53))

Our Web frontend needs to implement the participant interact interface for Alice, which we defined as:

@[code{28-32}](@reach-lang/examples/tut-9/index.rsh)

We will provide the `wager` and `deadline` values,
and define some button handlers in order to trigger the deployment of the contract.

@[code{56-72}](@reach-lang/examples/tut-9/index.js)

+ On line 59, we set the component state to display XXX (exviewref "tut-9" "SetWager").
+ On line 61, we define what to do when the user clicks the `Set Wager` button,
which is to set the component state to display XXX (exviewref "tut-9" "Deploy").
+ On lines 62 thru 69, we define what to do when the user clicks the `Deploy` button.
+ On line 63, we call `acc.deploy`, which triggers a deploy of the contract.
+ On line 64, we set the component state to display XXX (exviewref "tut-9" "Deploying").
+ On line 65, we set the `wager` property.
+ On line 66, we set the `deadline` property based on which connector is being used.
+ On line 67, we start running the Reach program as Alice, using the `this` React component
as the participant interact interface object.
+ On lines 68 and 69, we set the component state to display XXX (exviewref "tut-9" "WaitingForAttacher"),
which displays the deployed contract info as JSON.
+ On line 71, we render the appropriate view from [tut-9/views/DeployerViews.js](@github/examples/tut-9/views/DeployerViews.js).


XXX (exviewfigs
 "tut-9"
 "DeployerViews"
 '("Deploying" 55 61)
 '("WaitingForAttacher" 63 90))

--- 
XXX (exviewfigs "tut-9" "AttacherViews" '("Attach" 18 39) '("Attaching" 41 49))

Our Web frontend needs to implement the participant interact interface for Bob, which we defined as:

@[code{33-36}](@reach-lang/examples/tut-9/index.rsh)

We will provide the `acceptWager` callback,
and define some button handlers in order to attach to the deployed contract.

@[code{73-95}](@reach-lang/examples/tut-9/index.js)

+ On line 76, we initialize the component state to display XXX (exviewref "tut-9" "Attach").
+ On lines 78 thru 82, we define what happens when the user clicks the `Attach` button.
+ On line 79, we call `acc.attach`
+ On line 80, we set the component state to display XXX (exviewref "tut-9" "Attaching").
+ On line 81, we start running the Reach program as Bob, using the `this` React component
as the participant interact interface object.
+ On lines 83 thru 88, we define the `acceptWager` callback.
+ On lines 85 thru 87, we set the component state to display XXX (exviewref "tut-9" "AcceptTerms"),
and wait for a `Promise` which can be resolved via user interaction.
+ On lines 89 thru 92, we define what happens when the user clicks the `Accept Terms and Pay Wager` button:
the `Promise` from line 90 is resolved, and we set the component state to display XXX (exviewref "tut-9" "WaitingForTurn").
+ On line 93, we render the approprite view from [tut-9/views/AttacherViews.js](@github/examples/tut-9/views/AttacherViews.js)


XXX (exviewfigs
 "tut-9"
 "AttacherViews"
 '("AcceptTerms" 51 70)
 '("WaitingForTurn" 72 81))

--- 
@[code{96-96}](@reach-lang/examples/tut-9/index.js)

Finally, we call a small helper function from [tut-9/views/render.js](@github/examples/tut-9/views/render.js)
to render our App component.

--- 
As a convenience for running the React development server,
you can call:

```
$ ./reach react
```


---

To run the React development server with Algorand,
you can call:

```
$ REACH_CONNECTOR_MODE=ALGO ./reach react
```


Similarly, to run with Conflux:

```
$ REACH_CONNECTOR_MODE=CFX ./reach react
```


--- 
If you'd like to instead use Reach in your own JavaScript project,
you can call:

```
$ npm install @reach-sh/stdlib
```


::: note
The Reach standard library is undergoing continual improvement and is updated often.
If you are experiencing issues with the Node.js package, try updating!
:::

As usual, you can compile your Reach program `index.rsh` to the `backend` build artifact `build/index.main.mjs` with:

```
$ ./reach compile
```


--- 
Now our implementation of _Rock, Paper, Scissors!_ is live in the browser!
We can leverage callbacks in the participant interact interface to display to and gather information from the user,
through any Web UI framework of our choice.

If we wanted to deploy this application to the world, then we would take the static files that React produces and host them on a Web server.
These files embed your compiled Reach program, so there's nothing more to do than provide them to the world.

In [the next section](##tut-10), we'll summarize where we've gone and direct you to the next step of your journey to decentralized application mastery.

XXX (check:tf
 "True"
 "Reach integrates with all Web interface libraries, like React, Vue, and so on, because Reach frontends are just normal JavaScript programs.")

XXX (check:tf
 "True"
 "Reach accelerates your development with React by baking-in a React development server and the deployment process to test React programs locally.")

## {#tut-10} Onward and Further

Let's review what we've done through this tutorial:

+ In [part one](##tut-1), we saw how Reach can be installed with one command on almost any system without any dependencies beyond what most developers have anyways.
+ In [part two](##tut-2), we saw how Reach programs have a succinct setup that easily abstracts the details of your chosen consensus network into a couple lines and three key API calls.
+ In [part three](##tut-3), we saw how Reach allows developers to focus on the business logic of their decentralized application and look past the nitty-gritty details of blockchain interaction and protocol design.
+ In [part four](##tut-4), we saw that it is just as easy for Reach to deal with tokens and network transactions as it is to deal with data sharing.
+ In [part five](##tut-5), we introduce you to the Reach [automatic formal verification](##guide-assert) engine and its ability to ensure our program doesn't have entire categories of flaws and security vulnerabilities.
+ In [part six](##tut-6), we saw how Reach allows you to specify how to deal with [non-participation](##guide-timeout) and protect against funds being locked in contracts.
+ In [part seven](##tut-7), we saw how Reach can express arbitrary length interactions and how flexible the Reach frontends are to variations in the backend.
+ In [part eight](##tut-8), we saw how to decouple your Reach program from the Reach standard testing environment and launch an interactive version on a real network.
+ In [part nine](##tut-9), we saw how to deploy your Reach program as a fully decentralized Web application.


Despite having done so much, this is really just a brief introduction to what is possible with Reach.

How difficult was all this?
Let's look at the final versions of our programs.

First, let's look at the Reach program:

@[code](@reach-lang/examples/tut-8/index.rsh)

Next, the JavaScript command-line frontend:

@[code](@reach-lang/examples/tut-8/index.mjs)

And finally, the Web frontend:

@[code](@reach-lang/examples/tut-9/index.js)

We wrote about a hundred lines of Reach and two different frontends.
Our command-line version about a hundred lines of JavaScript.
While our Web version is about the same length, but has a lot of presentation code as well.

Behind the scenes, Reach generated hundreds of lines of Solidity (which you can look at here: [tut-8/build/index.main.sol](@github/examples/tut-8/build/index.main.sol)), almost two thousand lines of TEAL (which you can look at here: [tut-8/build/index.main.appApproval.teal](@github/examples/tut-8/build/index.main.appApproval.teal)), as well as over a thousand lines of JavaScript (which you can look at here: [tut-8/build/index.main.mjs](@github/examples/tut-8/build/index.main.mjs)).
If we weren't using Reach, then we'd have to write all this code ourselves and ensure that they are consistent and updated at every change to the application.

Now that you've seen an entire Reach application from beginning to end, it's time for you to start working on your own applications!

+ You may want to start [the workshop](##workshop), which is a self-study course on practicing and learning Reach through different specific projects.
+ Or, maybe you'd like to spend some time in [the guide](##guide) learning about the background of some of the concepts used in Reach programs.
+ Or, maybe it's time for you to dive into [the reference](##ref) and look into the minutiae of Reach's features.
+ Finally, you may like to repeat a portion of this tutorial, but using [a language other than JavaScript](##tut-7-rpc), like Python or Go!


No matter what you decide to read or work on next, we hope you'll join us on <CommunityLink />.
Once you join, message `@team, I just completed the tutorial!` and we'll give you the `tutorial veteran` role, so you can more easily help others work through it!

Thanks for spending your afternoon with us!


