#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application and assumes no prior experience with @|DApp|/blockchain development of any kind.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the other hand, if this is too simple, then you may want to start @seclink["workshop"]{the workshop} for larger and less contrained projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

If you're ready, click through to the @seclink["tut-1"]{first step}!

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "tut-1"]{Install and Initialize}
@(mint-scope "tut-1")

Reach is designed to work on POSIX systems with @link["https://en.wikipedia.org/wiki/Make_(software)"]{make}, @link["https://www.docker.com/get-started"]{Docker}, and @link["https://docs.docker.com/compose/install/"]{Docker Compose} installed.
The best way to install Docker on Mac and Windows is with @link["https://www.docker.com/products/docker-desktop"]{Docker Desktop}.

@margin-note{You probably already have @exec{make} installed.
For example, OS X and many other POSIX systems come with @exec{make}, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run @exec{sudo apt install make} to get it.}

You'll know that you have everything installed if you can run the following three commands without errors

@cmd{make --version}
@cmd{docker --version}
@cmd{docker-compose --version}

@margin-note{If you're using Windows, consult @seclink["guide-windows"]{the guide to using Reach on Windows}.}

Once you've confirmed that they are installed, choose a directory for this project. We recommend

@cmd{mkdir -p ~/reach/tut && cd ~/reach/tut}

Next, install Reach by downloading it from      @hyperlink["https://github.com/reach-sh/reach-lang"]{GitHub} by running

@cmd{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach}

You'll know that the download worked if you can run

@cmd{./reach version}

Since Reach is Dockerized, when you first use it, you'll need to download the images it uses.
This will happen automatically when you first use it, but you can do it manually now by running

@cmd{./reach update}

You'll know that everything is in order if you can run

@cmd{./reach compile --help}

@(hrule)

@margin-note{Get language support for Reach in your editor by visiting @seclink["guide-editor-support"].}

Now that your Reach installation is in order, you should open a text editor and get ready to @seclink["tut-2"]{write your first Reach application}!

@section[#:tag "tut-2"]{Scaffolding and Setup}
@(mint-scope "tut-2")

In this tutorial, we'll be building a version of @|RPS| where two players, @emph{Alice} and @emph{Bob}, can wager on the result of the game.
We'll start simple and slowly make the application more fully-featured.

You should follow along by copying each part of the program and seeing how things go.
If you're like us, you may find it beneficial to type each line out, rather than copying & pasting so you can start building your muscle memory and begin to get a sense for each part of a Reach program.

Let's start by creating a file named @exec{index.rsh}.
It doesn't matter where you put this file, but we recommend putting in the current directory, which would be @exec{~/reach/tut} if you're following along exactly.
In all the subsequent code samples, we'll label the files based on the chapter of the tutorial you're reading.
For example, start off by typing the following into @exec{index.rsh}:

@reachex[#:show-lines? #t "tut-2/index.rsh"
         #:link #t]

@margin-note{Did you notice that @reachin{export}, @reachin{const}, @reachin{exit}, and so on are links?
In Reach code samples, you can click on the names of keywords and standard library functions to be brought to their documentation.}

@margin-note{Did you notice that @reachexlink["tut-2/index.rsh"] was a link in the box above the code sample?
You can always click on these links to see the entire file in our @hyperlink["https://github.com/reach-sh/reach-lang"]{GitHub} repository.}

@margin-note{Did you notice the attractive clipboard icon on the top the right of that box?
You can click on it and the content of the code box will be copied onto your clipboard.}

@margin-note{Did your text editor recognize @exec{index.rsh} as a Reach program and give you proper syntax hightlighting?
If not, check if there's a plugin available for your editor by visiting @seclink["guide-editor-support"] or manually
configure it to treat Reach (@exec{.rsh}) files as JavaScript and things will be mostly correct.}

This is just a shell of a program that doesn't do much, but it has a few important components.

@itemlist[

@item{Line 1 indicates that this is a Reach program.
You'll always have this at the top of every program.}

@item{Line 3 defines the main export from the program.
When you compile, this is what the compiler will look at.}

@item{Line 6 specifies the two participants to this application, @emph{Alice} and @emph{Bob}.}

@item{Line 7 binds Reach identifiers (@reachin{A} and @reachin{B}) to these participants and defines the body of the program.}

]

Before we go too much further, let's create a similar shell for our JavaScript @tech{frontend} code.
Open a new file named @exec{index.mjs} and fill it with this:

@reachex[#:mode js
         #:show-lines? #t "tut-2/index.mjs"
         #:link #t]

@margin-note{Did you notice that @jsin{parseCurrency}, @jsin{newTestAccount}, @jsin{deploy}, and so on are links?
In JavaScript code samples, you can click on the names of standard library functions to be brought to their documentation.}

This JavaScript code is similarly schematic and will be consistent across all of your test programs.

@itemlist[

@item{Line 1 imports the Reach standard library loader.}

@item{Line 2 imports your backend, which @exec{./reach compile} will produce.}

@item{Line 4 defines an asynchronous function that will be the body of our frontend.}

@item{Line 5 loads the standard library dynamically based on the @envref{REACH_CONNECTOR_MODE} environment variable.}

@item{Line 6 defines a quantity of @tech{network token}s as the starting balance for each test account.}

@item{Lines 8 and 9 create test accounts with initial endowments for Alice and Bob.
This will only work on the Reach-provided developer testing network.}

@item{Line 11 has Alice deploy the application.}

@item{Line 12 has Bob attach to it.}

@item{Lines 15 through 18 initialize a backend for Alice.}

@item{Lines 19 through 22 initialize a backend for Bob.}

@item{Line 14 waits for the backends to complete.}

@item{Line 24 calls this asynchronous function that we've defined.}

]

This is now enough for Reach to compile and run our program. Let's try by running

@cmd{./reach run}

Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

@margin-note{The entire process that we just went through can be automated by running @cmd{./reach init} when you start your next project!}

In @seclink["tut-3"]{the next step}, we'll implement the logic of @|RPS| and our application will start doing something!

@(check:multi
  "2; Reach abstracts away the details of the underlying consensus network"
  "When you write a DApp using Reach, do you"
  "write a smart contract in Solidity, a backend in JavaScript using the Ethereum SDK, and a frontend in JavaScript, then use Reach to test and deploy it;"
  "write a program in Reach that generates a smart contract & a backend and a front-end in JavaScript, then use Reach to test and deploy it?")

@section[#:tag "tut-3"]{Rock, Paper, and Scissors}
@(mint-scope "tut-3")

In this section, we'll have Alice and Bob actually execute the game of @|RPS|.

We have to decide how to represent the hands of the game.
A simple way is to represent them as the numbers @reachin{0}, @reachin{1}, and @reachin{2}, standing for @litchar{Rock}, @litchar{Paper}, and @litchar{Scissors}.
However, Reach does not support unsigned integers of exactly two bits, so it is better to represent them as the equivalence class of integers modulo three, so we won't distinguish between @reachin{0} and @reachin{3} as @litchar{Rock}.

We'll use a similar strategy for representing the three outcomes of the game: @litchar{B wins}, @litchar{Draw}, and @litchar{A wins}.

The first step is to change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move that they will play, and later informed of the outcome of the game.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'skip 12 25 "      // ..."]

@itemlist[

@item{Lines 3 through 5 define a @tech{participant interact interface} that will be shared between the two players.
In this case, it provides two methods: @reachin{getHand}, which returns a number; and @reachin{seeOutcome}, which receives a number.}

@item{Line 10 uses this interface for both participants.
Because of this line, @reachin{interact} in the rest of the program will be bound to an object with methods corresponding to these actions, which will connect to the @tech{frontend} of the corresponding participant.}

]

Before continuing with the Reach application, let's move over to the JavaScript interface and implement these methods in our @tech{frontend}.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 14 36 "  // ..."]

@itemlist[

@item{Lines 14 and 15 define arrays to hold the meaning of the hands and outcomes.}

@item{Line 16 defines a constructor for the @jsin{Player} implementation.}

@item{Lines 17 through 21 define the @jsin{getHand} method.}

@item{Lines 22 through 24 define the @jsin{seeOutcome} method.}

@item{Finally, lines 30 and 34 instantiate the implementation once for Alice and once for Bob.
These are the actual objects that will be bound to @reachin{interact} in the Reach program.}

]

There should be nothing interesting or controversial about these implementations; that's the point of Reach: we get to just write normal business logic without worrying about the details of the @tech{consensus network} and decentralized application.

Let's return to the Reach program and look inside of the body of the program for what actions Alice and Bob take.

In a real-life game of @|RPS|, Alice and Bob simultaneously decide what hand they will play and reveal it at the same time.
"Simultaneity" is a complex concept that is hard to realize in practice.
For example, if you've ever player against a little kid, you may notice them trying to see what you're going to choose and delaying until the last minute to show their hand so they will win.
In a decentralized application, it is not possible to have simultaneity.
Instead, we have to select a particular participant who will "go first".
In this case, we'll choose Alice.

@margin-note{Does Alice go first, or do we call the player that goes first "Alice"?
This might seem like an unnecessary distinction to make, but it is a very subtle point about the way that Reach works.
In our @tech{frontend}, we explicitly ran @reachin{backend.Alice} and @reachin{backend.Bob}.
When we did that, we were committing that particular JavaScript thread to be either Alice or Bob.
In our game, whoever chose to run the Alice backend is the one that will go first.
In other words, @bold{Alice goes first}.
This will be more obvious at @seclink["tut-8"]{the end of the tutorial} when we'll make the choice interactively about which role to play.}

The game proceeds in three steps.

First, the backend for Alice interacts with its frontend, gets Alice's hand, and publishes it.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'only 12 15 "      // ..."]

@itemlist[

@item{Line 12 states that this block of code is something that @emph{only} @reachin{A} (i.e., Alice) performs.}

@item{That means that the variable, @reachin{handA}, bound on line 13 is known only to Alice.}

@item{Line 13 binds that value to the result of interacting with Alice through the @reachin{getHand} method, which we wrote in JavaScript.}

@item{Line 13 also @tech{declassifies} the value, because in Reach, all information from @tech{frontends} is @tech{secret} until it is explicitly made public.}

@item{Line 14 has Alice @tech{join} the application by publishing the value to the @tech{consensus network}, so it can be used to evaluate the outcome of the game.
Once this happens, the code is in a "@tech{consensus step}" where all participants act together.}

@item{Line 15 commits the state of the @tech{consensus network} and returns to "@tech{local step}" where individual participants can act alone.}

]

The next step is similar, in that Bob publishes his hand; however, we don't immediately commit the state, instead we compute the outcome of the game.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'only 17 22 "      // ..."]

@itemlist[

@item{Lines 17 through 19 match Alice's similar @tech{local step} and @tech{join}ing of the application through a @tech{consensus transfer} @tech{publication}.}

@item{But, line 21 computes the outcome of the game before committing.
(@reachin{(handA + (4 - handB)) % 3} is a clever equation to compute the winner of a game of @|RPS| using modular arithmetic.
Consider when @reachin{handA} is @reachin{0} (i.e., @litchar{Rock}) and @reachin{handB} is @reachin{2} (i.e., @litchar{Scissors}), then this equation is @reachin{((handA + (4 - handB)) % 3) = ((0 + (4 - 2)) % 3) = ((0 + 2) % 3) = (2 % 3) = 2}, which is the last outcome, that is @litchar{A wins}, as we expect it to be.)}

]

Finally, we use the @tech{each} form to have each of the participants send the final outcome to their frontends.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'only 24 25 "      // ..."]

@itemlist[

@item{Line 24 states that this is a @tech{local step} that @tech{each} of the participants performs.}

]

At this point, we can run the program and see its output by running

@cmd{./reach run}

Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

@verbatim{
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
}

Alice is pretty good at @|RPS|!

@tech{Consensus networks} in general, and Reach specifically, guarantee that all participants agree on the outcome of their decentralized computation.
Indeed, this is where the name @tech{consensus network} comes from, as they enable these distributed, and untrusted, parties to come to a consensus, or agreement, about the intermediate states of a computation; and if they agree on the intermediate states, they will also agree on the output.
That's why every time you run @exec{./reach run}, both Alice and Bob will see the same outcome!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-3/index.rsh"] and @reachexlink["tut-3/index.mjs"] to make sure you copied everything down correctly!}

In @seclink["tut-4"]{the next step}, we'll add some stakes to the game, because Alice needs to take her skills to the bank!

@(check:many
  @list{2 and 3; Reach programs specify a two-way interface between the frontend and the backend via the @tech{participant interact interface}.}
  "Reach programs allows interaction with a user interface through which of the following methods"
  "by forcing you to write a custom backend for the user interface that connects to the generated smart contract,"
  "by allowing the frontends to provide values directly to the Reach application,"
  "by allowing the Reach program to callback to the frontend via the interact object.")

@(check:multi
  @list{2; the @reachin{publish} primitive does everything for you.}
  "How do participants in a Reach application share information with each other and find out what what others have shared?"
  "Reach generates a smart contract, but you need to implement a process to scan the blockchain for events that corresponding to sharing;"
  @item{The Reach primitive @reachin{publish} allows a participant to share information with all other participants, which happens automatically without the other parties needing to do anything special;}
  @item{The Reach primitive @reachin{publish} allows a participant to share information with all other participants, but they need to explicitly run the receive primitive to receive published information.})

@section[#:tag "tut-4"]{Bets and Wagers}
@(mint-scope "tut-4")

Although it's fun to play @|RPS| with friends for a laugh, it's even better to play it with enemies and your entire life-savings on the line!
Let's change our program so that Alice can offer a wager to Bob and whoever wins will take the pot.

This time, let's start with changes to the JavaScript @tech{frontend} and then we'll go back into the Reach code and connect the new methods up.

Since we're going to be having funds get transfered, we'll record the balances of each participant before the game starts, so we can more clearly show what they won at the end.
We'll add this code in between account creation and contract deployment.

@reachex[#:mode js
         #:show-lines? #t "tut-4/index.mjs"
         #:link #t
         'only 5 13 "  // ..."]

@itemlist[

@item{Line 10 shows a helpful function for displaying currency amounts with up to 4 decimal places.}

@item{Line 11 shows a helpful function for getting the balance of a participant and displaying it with up to 4 decimal places.}

@item{Lines 12 and 13 get the balance before the game starts for both Alice and Bob.}

]

Next, we'll update Alice's interface object to include her wager.

@reachex[#:mode js
         #:show-lines? #t "tut-4/index.mjs"
         #:link #t
         'only 32 35 "    // ..."]

@itemlist[

@item{Line 33 splices the common @jsin{Player} interface into Alice's interface.}

@item{Line 34 defines her wager as @litchar{5} units of the @tech{network token}.
This is an example of using a concrete value, rather than a function, in a @tech{participant interact interface}.}

]

For Bob, we'll modify his interface to show the wager and immediately accept it by returning.

@reachex[#:mode js
         #:show-lines? #t "tut-4/index.mjs"
         #:link #t
         'only 36 41 "    // ..."]

@itemlist[

@item{Lines 38 through 40 define the @jsin{acceptWager} function.}

]

Finally, after the computation is over, we'll get the balance again and show a message summarizing the effect.

@reachex[#:mode js
         #:show-lines? #t "tut-4/index.mjs"
         #:link #t
         'only 44 48 "  // ..."]

@itemlist[

@item{Lines 44 and 45 get the balances afterwards.}

@item{Lines 47 and 48 print out the effect.}

]

These changes to the @tech{frontend} only deal with issues of presentation and interfacing.
The actual business logic of making the wager and transferring the funds will happen in the Reach code.

Let's look at that now.

First, we need to update the @tech{participant interact interface}.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'skip 18 41 "      // ..."]

@itemlist[

@item{Lines 6 through 8 define Alice's interface as the @reachin{Player} interface, plus an integer value called @reachin{wager}.}

@item{Lines 9 through 11 do the same for Bob, where he has a method called @reachin{acceptWager} that can look at the wager value.}

@item{Line 16 associates these interfaces with the corresponding participants.
The format of this line is a @tech{tuple} of @reachin{Participant} definitions, where the first argument is a string that names the @tech{backend} @tech{participant}
and the second argument is the @tech{participant interact interface}. It's conventional to name them similarly.}

]

Each of the three parts of the application have to be updated to deal with the wager.
Let's look at Alice's first step first.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 18 23 "      // ..."]

@itemlist[

@item{Line 19 has Alice @tech{declassify} the wager for transmission.}

@item{Line 21 is updated so that Alice shares the wager amount with Bob.}

@item{Line 22 has her transfer the amount as part of her @tech{publication}.
The Reach compiler would throw an exception if @reachin{wager} did not appear on line 21, but did appear on line 22.
Change the program and try it.
This is because the @tech{consensus network} needs to be able to verify that the amount of @tech{network tokens} included in Alice's @tech{publication} match some computation available to @tech{consensus network}.}

]

Next, Bob needs to be shown the wager and given the opportunity to accept it and transfer his funds.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 25 29 "      // ..."]

@itemlist[

@item{Line 26 has Bob accept the wager.
If he doesn't like the terms, his @tech{frontend} can just not respond to this method and the @|DApp| will stall.}

@item{Line 29 has Bob pay the wager as well.}

]

The @|DApp| is now running in a @tech{consensus step} and
the contract itself now holds twice the wager amount.
Before, it would compute the outcome and then commit the state; but now, it needs to look at the outcome and use it to balance the account.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 31 38 "      // ..."]

@itemlist[

@item{Lines 33 through 35 computes the amounts given to each participant depending on the outcome by determining how many @reachin{wager} amounts each party gets.
If the outcome is @reachin{2}, @litchar{Alice wins}, then she gets two portions; while if it is @reachin{0}, @litchar{Bob wins}, then he gets two portions; otherwise they each get one portion.}

@item{Lines 36 and 37 transfer the corresponding amounts.
This transfer takes place from the contract to the participants, not from the participants to each other, because all of the funds reside inside of the contract.}

@item{Line 38 commits the state of the application and allows the participants to see the outcome and complete.}

]

At this point, we can run the program and see its output by running

@cmd{./reach run}

Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

@verbatim{
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
}

@margin-note{How come Alice and Bob's balance goes back to @litchar{10} each time?
It's because every time we run @exec{./reach run}, it starts a completely fresh instance of the testing network and creates new accounts for each player.}

@margin-note{How come the balances aren't exactly @litchar{10}, @litchar{15}, and @litchar{5}?
It's because Ethereum transactions cost "gas" to run.

If we had shown all the decimals, they'd look like this:

@(hrule)

@verbatim{
Alice went from 10 to 14.999999999999687163.
Bob went from 10 to 4.999999999999978229.
...
Alice went from 10 to 4.999999999999687163.
Bob went from 10 to 14.999999999999978246.
}

@(hrule)

Why does Alice win slightly less than Bob when she wins?
She has to pay to @tech{deploy} the contract, because she calls @jsin{acc.deploy} in her @tech{frontend}.
The @seclink["guide-deploymode"]{guide section on deployment} discusses how to avoid this difference.}

Alice is doing okay, if she keeps this up, she'll make a fortune on @|RPS|!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-4/index.rsh"] and @reachexlink["tut-4/index.mjs"] to make sure you copied everything down correctly!}

Now that there is a reason to play this game, it turns out that there's a major security vulnerability.
We'll fix this in @seclink["tut-5"]{the next step}; make sure you don't launch with this version, or Alice is going to go broke!

@(check:multi
  @list{2; the @reachin{pay} and @reachin{transfer} primitives do everything for you.}
  "How do Reach programs manage token funds?"
  "They don’t; you need to manage them explicitly in parallel to the Reach program;"
  @item{The @reachin{pay} primitive can be added to a @reachin{publish} primitive to send funds to the Reach program, which can then use the @reachin{transfer} primitive to send funds back to participants, and other addresses.})

@section[#:tag "tut-5"]{Trust and Commitments}
@(mint-scope "tut-5")

In the last section, we made it so that Alice and Bob can actually exchange currency when they play @|RPS|.
However, the version of the application we wrote has a fundamental flaw: Bob can win every game!

How is that possible?
We showed executions of the game where Alice won, like the following

@verbatim{
$ ./reach run
Alice played Rock
Bob accepts the wager of 5.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10 to 14.9999.
Bob went from 10 to 4.9999.
}

The problem is that this version of the game only executed an @tech{honest} version of Bob, that is, one that followed the Reach program exactly, including in his private @tech{local steps}.
It is possible for a deviant and dis@tech{honest} version of a Bob @tech{backend} to execute different code and always win by computing the appropriate guess based on what value Alice provided for @reachin{handA}.

If we change Bob's code to the following:

@reachex[#:show-lines? #t "tut-5-attack/index.rsh"
         #:link #t
         'only 25 29 "      // ..."]

then he will ignore the @tech{frontend} and just compute the correct value.

If we run this version of the program, we will see output like this:

@verbatim{
$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10 to 4.9999.
Bob went from 10 to 14.9999.
}

In this version, unlike the @tech{honest} version, Bob never consults the @tech{frontend} and so it never prints out the message of what hand Bob played.
No matter what Alice chooses, Bob will always win.

@(hrule)

Is it just a fluke of the random number generator that we observed Bob always winning?
How would we know?
Reach comes with an    @seclink["guide-assert"]{automatic verification} engine that we can use to mathematically prove that this version will always result in the @reachin{outcome} variable equalling @reachin{0}, which means Bob wins.
We can instruct Reach to prove this theorem by adding these lines after computing the @reachin{outcome}:

@reachex[#:show-lines? #t "tut-5-attack/index.rsh"
         #:link #t
         'only 31 34 "      // ..."]

@itemlist[

@item{Line 32 requires that the dis@tech{honest} version of Bob be used for the proof.}

@item{Line 33 conducts the proof by including an @tech{assert} statement in the program.}

]

Before we had this line in the file, when we ran @exec{./reach run}, it would print out the message:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-4/index.txt"
         #:link #t
         'only 2 7 "      // ..."]

But now, it prints out

@reachex[#:mode verbatim
         #:show-lines? #t "tut-5-attack/index.txt"
         #:link #t
         'only 2 7 "      // ..."]

@itemlist[

@item{Line 7 is different and shows that more theorems have been proven about our program.
It prints out five more, rather than one more, because the theorem is proved differently in the different verification modes.}

]

@(hrule)

Many programming languages include @link["https://en.wikipedia.org/wiki/Assertion_(software_development)"]{assertions} like this, but Reach is one of a small category where the compiler doesn't just insert a runtime check for the property, but actually conducts a mathematical proof at compile-time that the expression @emph{always} evaluates to @reachin{true}.

In this case, we used Reach's @seclink["guide-assert"]{automatic verification} engine to prove that an attack did what we expected it would.
But, it is better to use verification to show that @emph{no flaw} exists and @emph{no attack} is possible.

Reach includes some such assertions automatically in every program.
That's why every version of @|RPS| has said that a number of theorems were checked.
We can see what these theorems do by deliberating inserting an error in the program.

Let's change the computation of the payout and make it so that if Alice wins, then she only gets her wager back, not Bob's.

@reachex[#:show-lines? #t "tut-5-attack/index-bad.rsh"
         #:link #t
         'only 34 41 "      // ..."]

@itemlist[

@item{Line 36 has @reachin{[0, 1]}, but should have @reachin{[0, 2]}.}

]

When we run @exec{./reach compile @reachexlink["tut-5-attack/index-bad.rsh"]}, it gives details about the error:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-5-attack/index-bad.txt"
         #:link #t
         'only 4 22 ""]

There's a lot of information in the compiler output that can help an experienced programmer track down the problem. But the most important parts are

@itemlist[

@item{Line 7 says that this is an attempt to prove the theorem that the balance at the end of the program is zero, which means that no @tech{network tokens} are sealed in the @tech{contract} forever.}

@item{Line 8 says that this happens when the program exits on line 45, which directs the programmer to that path through the program.}

@item{Lines 10-14 describe the values that could cause the theorem to fail.}

@item{Lines 16-21 outline the theorem that failed.}

]

These kinds of @seclink["guide-assert"]{automatic verifications} are helpful for Reach programmers, because they don't need to remember to put them in their program, and they will still be protected from entire categories of errors.

@(hrule)

However, now let's add an @tech{assert}ion to the program that will ensure that every version of the program that allows Bob to know Alice's hand before he chooses his own will be rejected.

We'll go back to the version of @reachexlink["tut-4/index.rsh"] from the last section, which has an @tech{honest} version of Bob.
(Click on the preceeding link if you need to see what it contained.)

We'll add a single line to the program after Alice publishes, but before Bob takes a @tech{local step}:

@reachex[#:show-lines? #t "tut-5-attack/index-fails.rsh"
         #:link #t
         'only 21 28 "      // ..."]

@itemlist[

@item{Line 25 contains a @tech{knowledge assertion} that Bob cannot know Alice's value @reachin{handA} at this point in the program.
In this case, it is obvious that this is not true, because Alice shares @reachin{handA} at line 21.
In many cases, this is not obvious and Reach's @seclink["guide-assert"]{automatic verification} engine has to reason about how values that Bob @emph{does know} are connected to values that might be related to Alice's secret values.}

]

When we run @exec{./reach run}, it reports that this assertion is false:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-5-attack/index-fails.txt"
         #:link #t
         'only 3 5 ""]

It is not enough to correct failures and attacks when you discover them.
You must @bold{always} add an assertion to your program that would fail to hold if the attack or failure were present.
This ensures that all similar attacks are not present and that they will not accidentally be reintroduced.

@(hrule)

Let's use these insights into @seclink["guide-assert"]{automatic verification} and rewrite our @|RPS| so that it is more trustworthy and secure.

Since we've been making lots of changes to the code, let's start fresh with a new version and we'll look at every single line again, to make sure that you aren't missing anything.

First, we'll define the rules of @|RPS| a little bit more abstractly, so we can separate the logic of the game from the details of the application:

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 1 7 "// ..."]

@itemlist[

@item{Line 1 is the usual Reach version header.}

@item{Lines 3 and 4 define @tech{enumeration}s for the hands that may be played, as well as the outcomes of the game.}

@item{Lines 6 and 7 define the function that computes the winner of the game.}

]

When we first wrote @|RPS|, we asked you to trust that this formula for computing the winner is correct, but it is good to actually check.
One way to check would be to implement a JavaScript @tech{frontend} that didn't interact with a real user, nor would it randomly generate values, but instead, it would return specific testing scenario values and check that the output is as expected.
That's a typical way to debug and is possible with Reach.
However, Reach allows us to write such test cases directly into the Reach program as verification assertions.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 9 11 "// ..."]

@itemlist[

@item{Line 9 makes an @tech{assert}ion that when Alice plays Rock and Bob plays Paper, then Bob wins as expected.}

]

But, Reach's @seclink["guide-assert"]{automatic verification} allows us to express even more powerful statements about our program's behavior.
For example, we can state that no matter what values are provided for @reachin{handA} and @reachin{handB}, @reachin{winner} will always provide a valid outcome:

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 13 15 "// ..."]

And we can specify that whenever the same value is provided for both hands, no matter what it is, @reachin{winner} always returns @reachin{DRAW}:

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 17 18 "// ..."]

These examples both use @reachin{forall}, which allows Reach programmers to quantify over all possible values that might be provided to a part of their program.
You might think that these theorems will take a very long time to prove, because they have to loop over all @(number->nice-string (expt 2 (* 256 3))) possibilities (e.g., Ethereum uses 256-bits for its unsigned integers) for the bits of @reachin{handA} (twice!) and @reachin{handB}.
In fact, on the author's MacBook Pro from early 2015, it takes less than half a second.
That's because Reach uses an advanced @seclink["guide-reach"]{symbolic execution engine} to reason about this theorem abstractly without considering individual values.

Let's continue the program by specifying the @tech{participant interact interface}s for Alice and Bob.
These will be mostly the same as before, except that we will also expect that each @tech{frontend} can provide access to random numbers.
We'll use these later on to protect Alice's hand.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 20 23 "// ..."]

The only line that is different is line 21, which includes @reachin{hasRandom}, from the Reach standard library, in the interface.

@reachex[#:mode js
         #:show-lines? #t "tut-5/index.mjs"
         #:link #t
         'only 20 30 "  // ..."]

Similarly, we only need to modify one line of our JavaScript @tech{frontend}.
Line 21 allows each @tech{participant}'s Reach code to generate random numbers as necessary.

These two changes might look identical, but they mean very different things.
The first, line 21 in the Reach program, adds @reachin{hasRandom} to the interface that the @tech{backend} expects the @tech{frontend} to provide.
The second, line 21 in the JavaScript, adds @reachin{hasRandom} to the implementation that the @tech{frontend} provides to the @tech{backend}.

We're now at the crucial juncture where we will implement the actual application and ensure that Alice's hand is protected until after Bob reveals his hand.
The simplest thing would be to have Alice just publish the wager, but this, of course, would just leave Bob vulnerable.
We need Alice to be able to publish her hand, but also keep it secret.
This is a job for a @link["https://en.wikipedia.org/wiki/Commitment_scheme"]{cryptographic commitment scheme}.
Reach's standard library comes with @reachin{makeCommitment} to make this easier for you.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 36 42 "      // ..."]

@itemlist[

@item{Line 37 has Alice compute her hand, but @emph{not} declassify it.}

@item{Line 38 has her compute a commitment to the hand.
It comes with a secret "salt" value that must be revealed later.}

@item{Line 39 has Alice declassify the commitment and her wager.}

@item{Line 40 has her publish them and with line 41 has her include the wager funds in the publication.}

]

At this point, we can state the @tech{knowledge assertion} that Bob can't know either the hand or the "salt" and continue with his part of the program.

@margin-note{It is important to include the salt in the commitment, so that multiple commitments to the same value are not identical.
Similarly, it is important not to share the salt until later, because if an attacker knows the set of possible values, they can enumerate them and compare with the result of the commitment and learn the value.}

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 44 50 "      // ..."]

@itemlist[

@item{Line 44 states the @tech{knowledge assertion}.}

@item{Lines 45 through 49 are unchanged from the original version.}

@item{Line 50 has the transaction commit, without computing the payout, because we can't yet, because Alice's hand is not yet public.}

]

We now return to Alice who can reveal her secrets.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 52 55 "      // ..."]

@itemlist[

@item{Line 53 has Alice declassify the secret information.}

@item{Line 54 has her publish it.}

@item{Line 55 checks that the published values match the original values.
This will always be the case with @tech{honest} participants, but dis@tech{honest} participants may violate this assumption.}

]

The rest of the program is unchanged from the original version, except that it uses the new names for the outcomes:

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 57 68 "      // ..."]

Since we didn't have to change the @tech{frontend} in any meaningful way, the output of running @exec{./reach run} is still the same as it ever was:

@verbatim{
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
}

Except now, behind the scenes, and without any changes to the frontend, Alice now takes two steps in program and Bob only takes one, and she is protected against Bob finding her hand and using it to ensure he wins!

When we compile this version of the application, Reach's @seclink["guide-assert"]{automatic formal verification} engine proves many theorems and protects us against a plethora of mistakes one might make when writing even a simple application like this.
Non-Reach programmers that try to write decentralized applications are on their own trying to ensure that these problems don't exist.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-5/index.rsh"] and @reachexlink["tut-5/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is secure and doesn't contain any exploits for either Alice or Bob to guarantee a win.
However, it still has a final category of mistake that is common in decentralized applications: @seclink["guide-timeout"]{non-participation}.
We'll fix this in @seclink["tut-6"]{the next step}; make sure you don't launch with this version, or Alice may decide to back out of the game when she knows she's going to lose!

@check:tf["False"]{Since blockchain programs run on a single, global, publicly-checked and certified consensus network, you don’t need to test them as much as normal software, which run on a wide variety of different platforms and operating systems.}

@check:tf["False"]{It is easy to write correct programs that handle financial information, and even if you make a mistake, blockchains support an "Undo" operation that allows you to rollback to earlier versions of the ledger to correct mistakes and recover lost funds.}

@check:tf["True"]{Reach provides automatic verifications to ensure that your program does not lose, lock away, or overspend funds and guarantees that your applications are free from entire categories of errors.}

@check:tf["True"]{Reach provides tools for you to add custom verifications to your program, like ensuring that information is known only to one party, or that your implementation of a sensitive algorithm is correct.}

@section[#:tag "tut-6"]{Timeouts and Participation}
@(mint-scope "tut-6")

In the last section, we removed a security vulnerability from @|RPS| that was a clear attack on the viability of the application.
In this section, we'll focus on a more subtle issue that is important and unique to decentralized applications: @seclink["guide-timeout"]{non-participation}.

Non-participation refers to the act of one party ceasing to continue playing their role in an application.

In traditional client-server programs, like a Web server, this would be the case of a client not sending any more requests to the server, or the server stopping sending responses to the client.
In these sorts of traditional programs, non-participation is an exceptional circumstance that normally leads to an error message for clients and, at most, a log entry for servers.
Sometimes traditional programs will need to recycle resources, like network ports, on non-participation, but they would have also needed to do that if the transaction ended by normal means.
In other words, for traditional client-server programs, it is not necessary for designers to meticulously consider the consequences of non-participation.

In contrast, decentralized applications must be carefully designed with an eye towards their behavior in the face of non-participation.
For example, consider what happens in our @|RPS| game if after Alice has paid her wager, Bob never accepts and the application doesn't continue.
In this case, Alice's @tech{network tokens} would be locked inside of the @tech{contract} and lost to her.
Similarly, if after Bob accepted and paid his wager, Alice stopped participating and never submitted her hand, then both their funds would be locked away forever.
In each of these cases, both parties would be greatly hurt and their fear of that outcome would introduce an additional cost to transacting, which would lower the value they got from participating in the application.
Of course, in a situation like @|RPS| this is unlikely to be an important matter, but recall that @|RPS| is a microcosm of decentralized application design.

@margin-note{Technically, in the first case, when Bob fails to start the application, Alice is not locked away from her funds: since Bob's identity is not fixed until after his first message, she could start another instance of the game as the Bob role and then she'd win all of the funds, less any transaction costs of the @tech{consensus network}.
In the second case, however, there would be no recourse for either party.}

In the rest of this section, we'll discuss how Reach helps address non-participation.
For a longer discussion, refer to @seclink["guide-timeout"]{the guide chapter on non-participation}.

@(hrule)

In Reach, non-participation is handled through a "timeout" mechanism whereby each @tech{consensus transfer} can be paired with a @tech{step} that occurs for all @tech{participants} if the @tech{originator} of the @tech{consensus transfer} fails to make the required @tech{publication} before a particular @tech{time}.
We'll integrate this mechanism into our version of @|RPS| and deliberately insert non-participation into our JavaScript testing program to watch the consequences play out.

First, we'll modify the @tech{participant interact interface} to allow the @tech{frontend} to be informed that a timeout occurred.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 20 24 "// ..."]

@itemlist[

@item{Line 24 introduces a new method, @reachin{informTimeout}, that receives no arguments and returns no information.
We'll call this function when a timeout occurs.}

]

We'll make a slight tweak to our JavaScript @tech{frontend} to be able to receive this message and display it on the console.

@reachex[#:mode js
         #:show-lines? #t "tut-6/index.mjs"
         #:link #t
         'only 20 33 "  // ..."]

Back in the Reach program, we'll define an identifier at the top of our program to use a standard deadline throughout the program.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 32 33 "// ..."]

@itemlist[

@item{Line 32 defines the deadline as ten @tech{time delta} units, which are an abstraction of the underlying notion of @tech{time} in the @tech{consensus network}.
In many networks, like Ethereum, this number is a number of blocks.}

]

Next, at the start of the Reach application, we'll define a helper function to inform each of the participants of the timeout by calling this new method.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 37 42 "    // ..."]

@itemlist[

@item{Line 38 defines the function as an @tech{arrow expression}.}

@item{Line 39 has each of the participants perform a @tech{local step}.}

@item{Line 40 has them call the new @reachin{informTimeout} method.}

]

We won't change Alice's first message, because there is no consequence to her non-participant: if she doesn't start the game, then no one is any worse off.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 46 47 "      // ..."]

However, we will adjust Bob's first message, because if he fails to participate, then Alice's initial wager will be lost to her.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 54 56 "      // ..."]

@itemlist[

@item{Line 56 adds a timeout handler to Bob's @tech{publication}.}

]

The timeout handler specifies that if Bob does not complete this action within a @tech{time delta} of @reachin{DEADLINE}, then the application transitions to @tech{step} given by the arrow expression.
In this case, the timeout code is a call to @reachin{closeTo}, which is a Reach standard library function that has Alice send a message and transfer all of the funds in the @tech{contract} to herself, then call the given function afterwards.
This means that if Bob fails to publish his hand, then Alice will take her @tech{network tokens} back.

We will add a similar timeout handler to Alice's second message.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 61 62 "      // ..."]

But in this case, Bob will be able to claim all of the funds if Alice doesn't participate.
You might think that it would be "fair" for Alice's funds to be returned to Alice and Bob's to Bob.
However, if we implemented it that way, then Alice would be wise to always timeout if she were going to lose, which she knows will happen, because she knows her hand and Bob's hand.

These are the only changes we need to make to the Reach program to make it robust against non-participation: seven lines!

@(hrule)

Let's modify the JavaScript @tech{frontend} to deliberately cause a timeout sometimes when Bob is supposed to accept the wager.

@reachex[#:mode js
         #:show-lines? #t "tut-6/index.mjs"
         #:link #t
         'only 35 51 "  // ..."]

@itemlist[

@item{Lines 42 through 50 redefine Bob's @jsin{acceptWager} method as an asynchronous function where half of the time it will take at least ten blocks on the Ethereum network by waiting for ten units of time to pass.
We know that ten is the value of @reachin{DEADLINE}, so this will cause a timeout.}

]

@(hrule)

Let's run the program and see what happens:

@verbatim{
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
}

Of course, when you run, you may not get two of the three times ending in a timeout.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-6/index.rsh"] and @reachexlink["tut-6/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is robust against either participant dropping from the game.
In @seclink["tut-7"]{the next step}, we'll extend the application to disallow draws and have Alice and Bob play again until there is a winner.

@(check:multi
  "4; Reach empowers programmers to design the application with the business logic they want."
  "What happens in a decentralized application when one participant refuses to take the next step of the program? For example, if Alice refuses to share her hand with Bob in a game of ‘Rock, Paper, Scissors’."
  "This is not possible, because the blockchain guarantees that each party performs a particular set of actions;"
  "The program hangs forever waiting for Alice to provide the value;"
  "Alice is punished and the program proceeds as-if Bob were the winner;"
  @item{It depends on how the program was written; if the developer used Reach, the default is (2), but the developer could include a @reachin{timeout} block to implement the (3) behavior.})

@section[#:tag "tut-7"]{Play and Play Again}
@(mint-scope "tut-7")

In this section, we extend our application so that Alice and Bob will continue to play against each other until there is a clear winner, so if it is a draw they will continue playing.

This will only require a change to the Reach program, not the JavaScript @tech{frontend}, but we will take the opportunity to modify the @tech{frontend} so that timeouts can happen to both parties when they are asked to submit their hands.
Let's do that to get it out of the way and not distract from the main task of removing draws.

We'll modify the @jsin{Player} interact object so that it will have a different @jsin{getHand} method.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 20 39 "  // ..."]

@itemlist[

@item{Lines 25 through 30 moves the forced timeout code that we wrote for Bob's @jsin{acceptWager} function into this method.
We also change the threshold so that timeouts only happen 1% of the time.
This isn't a very interesting behavior, so we'll make it much less frequent.}

]

We also adjust Bob's @jsin{acceptWager} function to remove the timeout code, since we're testing that differently now.
It's just a matter of reverting to the simpler version from before.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 41 52 "  // ..."]

@itemlist[

@item{Lines 48 through 50 have the simpler @jsin{acceptWager} method for Bob.}

]

@(hrule)

Now, let's look at the Reach application.
All of the details about the playing of the game and the interface to the players will remain the same.
The only thing that's going to be different is the order the actions take place.

It used to be that the steps were:

@itemlist[
#:style 'ordered

@item{Alice sends her wager and commitment.}

@item{Bob accepts the wager and sends his hand.}

@item{Alice reveals her hand.}

@item{The game ends.}

]

But, now because the players may submit many hands, but should only  have a single wager, we'll break these steps up differently, as follows:

@itemlist[
#:style 'ordered

@item{Alice sends her wager.}

@item{Bob accepts the wager.}

@item{Alice sends her commitment.}

@item{Bob sends his hand.}

@item{Alice reveals her hand.}

@item{If it's draw, return to step 3; otherwise, the game ends.}

]

Let's make these changes now.

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 42 46 "      // ..."]

@itemlist[

@item{Line 44 has Alice publish and pay the wager.}

]

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 48 52 "      // ..."]

@itemlist[

@item{Line 50 has Bob pay the wager.}

@item{Line 52 does @bold{not} have this @tech{consensus step} commit.}

]

@(hrule)

It's now time to begin the repeatable section of the application, where each party will repeatedly submit hands until the the outcome is not a draw.
In normal programming languages, such a circumstance would be implemented with a @jsin{while} loop, which is exactly what we'll do in Reach.
However, @reachin{while} loops in Reach require extra care, as discussed in @seclink["guide-loop-invs"]{the guide on loops in Reach}, so we'll take it slow.

In the rest of a Reach program, all identifier bindings are static and unchangable, but if this were the case throughout all of Reach, then @reachin{while} loops would either never start or never terminate, because the loop condition would never change.
So, a @reachin{while} loop in Reach can introduce a variable binding.

Next, because of Reach's @seclink["guide-assert"]{automatic verification} engine, we must be able to make a statement about what properties of the program are invariant before and after a @reachin{while} loop body's execution, a so-called @seclink["guide-loop-invs"]{"loop invariant"}.

Finally, such loops @emph{may only occur} inside of @tech{consensus steps}.
That's why Bob's transaction was not committed, because we need to remain inside of the consensus to start the @reachin{while} loop.
This is because all of the @tech{participants} must agree on the direction of control flow in the application.

Here's what the structure looks like:

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 53 55 "      // ..."]

@itemlist[

@item{Line 53 defines the loop variable, @reachin{outcome}.}

@item{Line 54 states the invariant that the body of the loop does not change the balance in the @tech{contract} account and that  @reachin{outcome} is a valid outcome.}

@item{Line 55 begins the loop with the condition that it continues as long as the outcome is a draw.}

]

Now, let's look at the body of the loop for the remaining steps, starting with Alice's commitment to her hand.

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 56 64 "        // ..."]

@itemlist[

@item{Line 56 commits the last transaction, which at the start of the loop is Bob's acceptance of the wager, and at subsequent runs of the loop is Alice's publication of her hand.}

@item{Lines 58 through 64 are almost identical to the older version, except the wager is already known and paid.}

]

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 66 71 "        // ..."]

Similarly, Bob's code is almost identical to the prior version, except that he's already accepted and paid the wager.

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 73 77 "        // ..."]

Alice's next step is actually identical, because she is still revealing her hand in exactly the same way.

Next is the end of the loop.

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 79 80 "        // ..."]

@itemlist[

@item{Line 79 updates the @reachin{outcome} loop variable with the new value.}

@item{Line 80 continues the loop.
Unlike most programming languages, Reach @bold{requires} that @reachin{continue} be explicitly written in the loop body.}

]

The rest of the program could be exactly the same as it was before, except now it occurs outside of the loop, but we will simplify it, because we know that the outcome can never be a draw.

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 82 88 "      // ..."]

@itemlist[

@item{Line 82 asserts that the outcome is never draw, which is trivially true because otherwise the @reachin{while} loop would not have exitted.}

@item{Line 83 transfers the funds to the winner.}

]

@(hrule)

Let's run the program and see what happens:

@verbatim{
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
}

As usual, your results may differ, but you should be able to see single round victories like this, as well as multi-round fights and timeouts from either party.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-7/index.rsh"] and @reachexlink["tut-7/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| will always result in a pay-out, which is much more fun for everyone.
In @seclink["tut-8"]{the next step}, we'll show how to exit "testing" mode with Reach and turn our JavaScript into an interactive @|RPS| game with real users.

@(check:multi
  @list{2; Reach supports @reachin{while} loops.}
  "How do you write an application in Reach that runs arbitrarily long, like a game of Rock, Paper, Scissors that is guaranteed to not end in a draw?"
  "This is not possible, because all Reach programs are finitely long;"
  @item{You can use a @reachin{while} loop that runs until the outcome of the game is decided.})

@(check:many
  @list{All of the above.}
  @list{When you check if a program with a @reachin{while} loop is correct, you need to have a property called a loop invariant. Which of the following statements have to be true about the loop invariant?}
  @item{The part of the program before the @reachin{while} loop must establish the invariant.}
  "The condition and the body of the loop must establish the invariant."
  "The negation of the condition and the invariant must establish any properties of the rest of the program.")

@section[#:tag "tut-8"]{Interaction and Independence}
@(mint-scope "tut-8")

In the last section, we made our @|RPS| run until there was a definitive winner.
In this section, we won't be making any changes to the Reach program itself.
Instead, we'll go under the covers of @exec{reach run}, as well as build a version of our game that is interactive and can be played away from a private developer test network.

@(hrule)

In the past, when we've run @exec{./reach run}, it would create a Docker image just for our Reach program that contained a temporary Node.js package connecting our JavaScript @tech{frontend} to the Reach standard library and a fresh instance of a private developer test network.
Since in this section, we will customize this and build a non-automated version of @|RPS|, as well as give the option to connect to a real Ethereum network.

We'll start by running

@cmd{./reach scaffold}

which will automatically generate the following files for us:

@itemlist[

@item{@exec{package.json} --- A Node.js package file that connects our @exec{index.mjs} to the Reach standard library.}

@item{@exec{Dockerfile} --- A Docker image script that builds our package efficiently and runs it.}

@item{@exec{docker-compose.yml} --- A Docker Compose script that connects our Docker image to a fresh instance of the Reach private developer test network.}

@item{@exec{Makefile} --- A @exec{Makefile} that easily rebuilds and runs the Docker image.}

]

We're going to leave the first two files unchanged.
You can look at them at @reachexlink["tut-8/package.json"] and @reachexlink["tut-8/Dockerfile"], but the details aren't especially important.
However, we'll customize the other two files.

First, let's look at the @reachexlink["tut-8/docker-compose.yml"] file:

@reachex[#:mode yaml
         #:show-lines? #t "tut-8/docker-compose.yml"
         #:link #t]

@itemlist[

@item{Lines 2 and 3 define a service for starting our application.
Your line 3 will say @litchar{tut}, rather than @litchar{tut-8}, if you've stayed in the same directory througout the tutorial.}

@item{Lines 5 and 6 define the Reach private developer test network service for Conflux.}

@item{Lines 7 and 8 define the Reach private developer test network service for Ethereum.}

@item{Lines 9 through 26 define the Reach private developer test network service for Algorand.}

@item{Lines 27 through 82 define services that allow the application to be run with different networks; including line 27, which defines @litchar{reach-app-tut-8-ETH-live} for connecting to a live network.}

@item{We'll also add lines 85 through 90 to define a @litchar{player} service that is our application with an open standard input, as well as two instances named @litchar{alice} and @litchar{bob}.}
]

With these in place, we can run

@cmd{docker-compose run WHICH}

where @exec{WHICH} is @litchar{reach-app-tut-8-ETH-live} for a live instance, or @litchar{alice} or @litchar{bob} for a test instance.
If we use the live version, then we have to define the environment variable @envref{ETH_NODE_URI} as the URI of our Ethereum node.

We'll modify the @reachexlink["tut-8/Makefile"] to have commands to run each of these variants:

@reachex[#:mode makefile
         #:show-lines? #t "tut-8/Makefile"
         #:link #t
         'only 34 44 ""]

However, if we try to run either of these, it will do the same thing it always has: create test accounts for each user and simulate a random game.
Let's modify the JavaScript @tech{frontend} and make them interactive.

@(hrule)

We'll start from scratch and show every line of the program again.
You'll see a lot of similarity between this and the last version, but for completeness, we'll show every line.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 1 6       "  // ..."]

@itemlist[

@item{Lines 1 and 2 are the same as before: importing the standard library and the backend.}

@item{Line 3 is new and imports a helpful library for simple console applications called @exec{ask.mjs} from the Reach standard library.
We'll see how these three functions are used below.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 7 12 "  // ..."]

@itemlist[

@item{Lines 7 through 10 ask the question whether they are playing as Alice and expect a "Yes" or "No" answer.
@jsin{ask} presents a prompt and collects a line of input until its argument does not error.
@jsin{yesno} errors if it is not given "y" or "n".}

]

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 13 29 "  // ..."]

@itemlist[

@item{Lines 16 through 19 present the user with the choice of creating a test account if they can or inputing a secret to load an existing account.}

@item{Line 21 creates the test account as before.}

@item{Line 27 loads the existing account.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 30 46 "  // ..."]

@itemlist[

@item{Lines 31 through 34 ask if the participant will deploy the contract.}

@item{Lines 36 through 38 deploy it and print out public information (@jsin{ctc.getInfo}) that can be given to the other player.}

@item{Lines 40 through 44 request, parse, and process this information.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 47 54 "  // ..."]

Next we define a few helper functions and start the participant interaction interface.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 55 59 "  // ..."]

First we define a timeout handler.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 60 79 "  // ..."]

Next, we request the wager amount or define the @jsin{acceptWager} method, depending on if we are Alice or not.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 80 97 "  // ..."]

Next, we define the shared @jsin{getHand} method.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 98 102 "  // ..."]

Finally, the @jsin{seeOutcome} method.

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t
         'only 103 111 "  // ..."]

Lastly, we choose the appropriate backend function and await its completion.

@(hrule)

We can now run

@cmd{make build}

to rebuild the images, then

@cmd{make run-alice}

in one terminal in this directory and

@cmd{make run-bob}

in another terminal in this directory.

Here's an example run:

@verbatim{
$ make run-alice
Are you Alice?
y
Starting Rock, Paper, Scissors as Alice
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the contract? (y/n)
y
The contract is deployed as = {"address":"0xc2a875afbdFb39b1341029A7deceC03750519Db6","creation_block":18,"args":[],"value":{"type":"BigNumber","hex":"0x00"},"creator":"0x2486Cf6C788890885D71667BBCCD1A783131547D"}
Your balance is 999.9999
How much do you want to wager?
10
What hand will you play?
r
You played Rock
The outcome is: Bob wins
Your balance is now 989.9999}

and

@verbatim{
$ make run-bob
Are you Alice?
n
Starting Rock, Paper, Scissors as Bob
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the contract? (y/n)
n
Please paste the contract information:
{"address":"0xc2a875afbdFb39b1341029A7deceC03750519Db6","creation_block":18,"args":[],"value":{"type":"BigNumber","hex":"0x00"},"creator":"0x2486Cf6C788890885D71667BBCCD1A783131547D"}
Your balance is 1000
Do you accept the wager of 10?
y
What hand will you play?
p
You played Paper
The outcome is: Bob wins
Your balance is now 1009.9999}

Of course, when you run the exact amounts and addresses may be different.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-8/index.rsh"], @reachexlink["tut-8/index.mjs"], @reachexlink["tut-8/package.json"], @reachexlink["tut-8/Dockerfile"], @reachexlink["tut-8/docker-compose.yml"], and @reachexlink["tut-8/Makefile"] to make sure you copied everything down correctly!}

@(hrule)

If we were to edit @reachexlink["tut-8/docker-compose.yml"], and move the @litchar{&default-app} on line 34 to line 54, then instead of running on Ethereum, we'd be able to test and run our application on Algorand.

@margin-note{We may need to also change line 32 of @reachexlink["tut-8/index.rsh"] that defines @reachin{DEADLINE} to be @reachin{10} to a higher number, like @reachin{30}.
This is because Algorand does not support an input-enabled developer network that only runs rounds when transactions are present, so it is possible that timeouts will occur unexpectedly.
We've commonly observed this on machines under heavy CPU load.}

@(hrule)

Now our implementation of @|RPS| is finished!
We are protected against attacks, timeouts, and draws, and we can run interactively on non-test networks.

In this step, we made a command-line interface for our Reach program.
In @seclink["tut-9"]{the next step}, we'll replace this with a Web interface for the same Reach program.

@check:tf["False; Reach does not impose any constraints on what kind of frontend is attached to your Reach application."]{Reach helps you build automated tests for your decentralized application, but it doesn’t support building interactive user-interfaces.}

@section[#:tag "tut-9"]{Web Interaction}
@author[(author+email "Dan Burton" "dan@reach.sh")]
@(mint-scope "tut-9")

In the last section, we made @|RPS| run as a command-line application, without any changes to the Reach program.
In this section, we again won't be making any changes to the Reach program.
Instead, we'll replace the command-line interface with a Web interface.

We will use @link["https://reactjs.org/"]{React.js} for this tutorial, but the same principles apply to any Web framework.

@margin-note{If you've never used React before, here are some basics about how it works:
@itemlist[
@item{React programs are JavaScript programs that use a special library that allows you to mix HTML inside of the body of your JavaScript.}
@item{React has a special compiler that combines a bundle of JavaScript programs, and all their dependencies, into one large file that can be deployed on a static Web server.
This is called "packing".}
@item{When you're developing and testing with React, you run a special development Web server that watches and updates this packed file every time you modify a source file, so you don't have to constantly run the compiler.}
@item{Reach automates the process of starting this development server for you when you run @exec{./reach react} and gives you access to it at @tt{http://localhost:3000/}.}
]}

Similarly, in this tutorial, we assume that we will be deploying (and testing) with Ethereum.
Reach Web applications rely on the Web browser to provide access to a consensus network account and its associated wallet.
On Ethereum, the standard wallet is @link["https://metamask.io"]{MetaMask}.
If you want to test this code, you'll need to install it and set it up.
Furthermore, MetaMask does not support multiple active accounts, so if you want to test @|RPS| locally, you'll need to have two separate browser instances: one to act as Alice and another to act as Bob.

@(hrule)

The code in this section does not use the scaffolding from the previous section.
Reach comes with a convenience command for deleting scaffolded files:

@cmd{./reach unscaffold}

Similarly, you do not need the previous @tt{index.mjs} file, because we'll be writing it completely from scratch to use React.
You can run the following command to delete it:

@cmd{rm index.mjs}

Or, you can copy the @tt{index.rsh} file into a new directory and work from there.

@(hrule)

This code is supplemented with @reachexlink["tut-9/index.css" "index.css"]
and some @reachexlink["tut-9/views" "views"].
These details are not specific to Reach, and are fairly trivial,
so we will not explain the specifics of those files.
If you run this locally, you'll want to download those files.
Your directory should look like:

@verbatim{
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
}

@(hrule)

We will focus on @reachexlink["tut-9/index.js"],
because @reachexlink["tut-9/index.rsh"] is the same as previous sections.

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 1 9 "// ..."]

On lines 1 thru 6, we import our view code and CSS.
On line 7, we import the compiled @reachin{backend}.
On line 8, we import the @reachin{stdlib} as @reachin{reach}.

@(hrule)

To run on Algorand, change the import on line 8.

@jsin{import * as reach from '@"@"reach-sh/stdlib/ALGO'}

@margin-note{
If you would like to use @jsin{loadStdlib},
you should call it like so:

@jsin{const reach = await loadStdlib(process.env)}

React compiles the Reach standard libray in such a way that
it does not have direct access to the environment variables
which are used to select the desired standard library.
This is why you need to pass @jsin{process.env} as an argument
to achieve the desired effect.
}

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 10 14 "// ..."]

On these lines we define a few helpful constants and defaults for later,
some corresponding to the enumerations we defined in Reach.

@(hrule) @;; Explain App

We start defining @jsin{App} as a React component,
and tell it what to do once it mounts, which is the React term for starting.

@exviewfigs["tut-9" "AppViews"
  '["ConnectAccount" 19 28]]

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 15 31 "// ..."]

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 39 41 "// ..."]

@itemlist[
 @item{On line 18, we initialize the component state to display @exviewref["tut-9" "ConnectAccount"].}
 @item{On lines 20 thru 31, we hook into React's @jsin{componentDidMount} lifecycle event, which is called when the component starts.}
 @item{On line 21, we use @jsin{getDefaultAccount}, which accesses the default browser account.
  For example, when used with Ethereum, it can discover the currently-selected MetaMask account.}
 @item{On line 26, we use @jsin{getFaucet} to try and access the Reach developer testing network faucet.}
 @item{On line 27, if @jsin{getFaucet} was successful, we set the component state to display @exviewref["tut-9" "FundAccount"].}
 @item{On line 29, if @jsin{getFaucet} was unsuccessful, we set the component state to skip to @exviewref["tut-9" "DeployerOrAttacher"].}
 @item{On line 39, we render the appropriate view from @reachexlink{tut-9/views/AppViews.js}.}
]

@exviewfigs["tut-9" "AppViews"
  '["FundAccount" 30 54]]

Next, we define callbacks on @jsin{App} for what to do when the user clicks certain buttons.

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 32 36 " . // ..."]

@itemlist[
  @item{On lines 32 thru 35, we define what happens when the user clicks the @litchar{Fund Account} button.}
  @item{On line 33, we transfer funds from the faucet to the user's account.}
  @item{On line 34, we set the component state to display @exviewref["tut-9" "DeployerOrAttacher"].}
  @item{On line 36, we define what to do when the user clicks the @litchar{Skip} button,
   which is to set the component state to display @exviewref["tut-9" "DeployerOrAttacher"].}
]

@exviewfigs["tut-9" "AppViews"
  '["DeployerOrAttacher" 56 78]]

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 37 38 "// ..."]

On lines 37 and 38, we set a sub-component
based on whether the user clicks @litchar{Deployer} or @litchar{Attacher}.

@(hrule) @;; Explain Player

Next, we will define @jsin{Player} as a React component,
which will be extended by the specialized components for Alice and Bob.

@exviewfigs["tut-9" "PlayerViews"
  '["GetHand" 8 32]]

Our Web frontend needs to implement the @tech{participant interact interface} for players, which we defined as:

@reachex[#:show-lines? #t "tut-9/index.rsh"
         #:link #t
         'only 20 24 "// ..."]

We will provide these callbacks via the React component directly.

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 42 55 "// ..."]

@itemlist[
 @item{On line 43, we provide the @jsin{random} callback}
 @item{On lines 44 thru 50, we provide the @jsin{getHand} callback.}
 @item{On lines 45 thru 47, we set the component state to display @exviewref["tut-9" "GetHand"],
  and wait for a @jsin{Promise} which can be resolved via user interaction.}
 @item{On line 48, which occurs after the @jsin{Promise} is resolved,
  we set the component state to display @exviewref["tut-9" "WaitingForResults"].}
 @item{On lines 51 and 52, we provide the @jsin{seeOutcome} and @jsin{informTimeout} callbacks,
  which set the component state to display @exviewref["tut-9" "Done"] and @exviewref["tut-9" "Timeout"], respectively.}
 @item{On line 53, we define what happens when the user clicks @litchar{Rock}, @litchar{Paper}, or @litchar{Scissors}:
  The @jsin{Promise} from line 45 is resolved.}
]

@exviewfigs["tut-9" "PlayerViews"
  '["WaitingForResults" 34 42]
  '["Done" 44 54]
  '["Timeout" 56 64]]

@(hrule) @;; explain Deployer

@;; TODO: rename Deployer->Alice, Attacher->Bob
Next, we will define @jsin{Deployer} as a React component for Alice,
which extends @jsin{Player}.

@exviewfigs["tut-9" "DeployerViews"
  '["SetWager" 20 38]
  '["Deploy" 40 53]]

Our Web frontend needs to implement the @tech{participant interact interface} for Alice, which we defined as:

@reachex[#:show-lines? #t "tut-9/index.rsh"
         #:link #t
         'only 25 27 "// ..."]

We will provide the @jsin{wager} value,
and define some button handlers in order to trigger the deployment of the contract.

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 56 72 "// ..."]

@itemlist[
 @item{On line 59, we set the component state to display @exviewref["tut-9" "SetWager"].}
 @item{On line 61, we define what to do when the user clicks the @litchar{Set Wager} button,
  which is to set the component state to display @exviewref["tut-9" "Deploy"].}
 @item{On lines 62 thru 69, we define what to do when the user clicks the @litchar{Deploy} button.}
 @item{On line 63, we call @jsin{acc.deploy}, which triggers a deploy of the contract.}
 @item{On line 64, we set the component state to display @exviewref["tut-9" "Deploying"].}
 @item{On line 65, we set the @jsin{wager} property.}
 @item{On line 66, we start running the Reach program as Alice, using the @jsin{this} React component
  as the @tech{participant interact interface} object.}
 @item{On lines 67 and 68, we set the component state to display @exviewref["tut-9" "WaitingForAttacher"],
  which displays the deployed contract info as JSON.}
 @item{On line 70, we render the appropriate view from @reachexlink{tut-9/views/DeployerViews.js}.}
]

@exviewfigs["tut-9" "DeployerViews"
  '["Deploying" 55 61]
  '["WaitingForAttacher" 63 90]]

@(hrule) @;; Explain Attacher

@exviewfigs["tut-9" "AttacherViews"
  '["Attach" 18 39]
  '["Attaching" 41 49]]

Our Web frontend needs to implement the @tech{participant interact interface} for Bob, which we defined as:

@reachex[#:show-lines? #t "tut-9/index.rsh"
         #:link #t
         'only 28 30 "// ..."]

We will provide the @jsin{acceptWager} callback,
and define some button handlers in order to attach to the deployed contract.

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 73 95 "// ..."]

@itemlist[
 @item{On line 76, we initialize the component state to display @exviewref["tut-9" "Attach"].}
 @item{On lines 78 thru 82, we define what happens when the user clicks the @litchar{Attach} button.}
 @item{On line 79, we call @jsin{acc.attach}}
 @item{On line 80, we set the component state to display @exviewref["tut-9" "Attaching"].}
 @item{On line 81, we start running the Reach program as Bob, using the @jsin{this} React component
  as the @tech{participant interact interface} object.}
 @item{On lines 83 thru 88, we define the @jsin{acceptWager} callback.}
 @item{On lines 85 thru 87, we set the component state to display @exviewref["tut-9" "AcceptTerms"],
  and wait for a @jsin{Promise} which can be resolved via user interaction.}
 @item{On lines 89 thru 92, we define what happens when the user clicks the @litchar{Accept Terms and Pay Wager} button:
  the @jsin{Promise} from line 90 is resolved, and we set the component state to display @exviewref["tut-9" "WaitingForTurn"].}
 @item{On line 93, we render the approprite view from @reachexlink{tut-9/views/AttacherViews.js}}
]

@exviewfigs["tut-9" "AttacherViews"
  '["AcceptTerms" 51 70]
  '["WaitingForTurn" 72 81]]

@(hrule) @;; explain renderDOM

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t
         'only 96 96 "// ..."]

Finally, we call a small helper function from @reachexlink{tut-9/views/render.js}
to render our App component.

@(hrule) @;; explain reach react and reach react-down

As a convenience for running the React development server,
you can call:

@cmd{./reach react}

@(hrule)

To run the React development server with Algorand,
you can call:

@cmd{REACH_CONNECTOR_MODE=ALGO ./reach react}

@(hrule) @;; explain npm install

If you'd like to instead use Reach in your own JavaScript project,
you can call:

@cmd{npm install @"@"reach-sh/stdlib}

@margin-note{The Reach standard library is undergoing continual improvement and is updated often.
If you are experiencing issues with the Node.js package, try updating!}

As usual, you can compile your Reach program @litchar{index.rsh} to the @jsin{backend} build artifact @litchar{build/index.main.mjs} with:

@cmd{./reach compile}

@(hrule) @;; conclusion

Now our implementation of @|RPS| is live in the browser!
We can leverage callbacks in the @tech{participant interact interface} to display to and gather information from the user,
through any Web UI framework of our choice.

If we wanted to deploy this application to the world, then we would take the static files that React produces and host them on a Web server.
These files embed your compiled Reach program, so there's nothing more to do than provide them to the world.

In @seclink["tut-10"]{the next section}, we'll summarize where we've gone and direct you to the next step of your journey to decentralized application mastery.

@check:tf["True"]{Reach integrates with all Web interface libraries, like React, Vue, and so on, because Reach frontends are just normal JavaScript programs.}

@check:tf["True"]{Reach accelerates your development with React by baking-in a React development server and the deployment process to test React programs locally.}

@section[#:tag "tut-10"]{Onward and Further}
@(mint-scope "tut-10")

Let's review what we've done through this tutorial:

@itemlist[

@item{In @seclink["tut-1"]{part one}, we saw how Reach can be installed with one command on almost any system without any dependencies beyond what most developers have anyways.}

@item{In @seclink["tut-2"]{part two}, we saw how Reach programs have a succinct setup that easily abstracts the details of your chosen @tech{consensus network} into a couple lines and three key API calls.}

@item{In @seclink["tut-3"]{part three}, we saw how Reach allows developers to focus on the business logic of their decentralized application and look past the nitty-gritty details of blockchain interaction and protocol design.}

@item{In @seclink["tut-4"]{part four}, we saw that it is just as easy for Reach to deal with tokens and network transactions as it is to deal with data sharing.}

@item{In @seclink["tut-5"]{part five}, we introduce you to the Reach @seclink["guide-assert"]{automatic formal verification} engine and its ability to ensure our program doesn't have entire categories of flaws and security vulnerabilities.}

@item{In @seclink["tut-6"]{part six}, we saw how Reach allows you to specify how to deal with @seclink["guide-timeout"]{non-participation} and protect against funds being locked in contracts.}

@item{In @seclink["tut-7"]{part seven}, we saw how Reach can express arbitrary length interactions and how flexible the Reach @tech{frontends} are to variations in the @tech{backend}.}

@item{In @seclink["tut-8"]{part eight}, we saw how to decouple your Reach program from the Reach standard testing environment and launch an interactive version on a real network.}

@item{In @seclink["tut-9"]{part nine}, we saw how to deploy your Reach program as a fully decentralized Web application.}

]

Despite having done so much, this is really just a brief introduction to what is possible with Reach.

How difficult was all this?
Let's look at the final versions of our programs.

First, let's look at the Reach program:

@reachex[#:show-lines? #t "tut-8/index.rsh"
         #:link #t]

Next, the JavaScript command-line frontend:

@reachex[#:mode js
         #:show-lines? #t "tut-8/index.mjs"
         #:link #t]

And finally, the Web frontend:

@reachex[#:mode js
         #:show-lines? #t "tut-9/index.js"
         #:link #t]

We wrote @exloc["tut-8/index.rsh"] lines of Reach and two different frontends.
Our command-line version is @exloc["tut-8/index.mjs"] lines of JavaScript, or @exloc["tut-8/index.rsh" "tut-8/index.mjs"] lines together.
While our Web version is @exloc["tut-9/index.js"] lines of JavaScript, or @exloc["tut-9/index.rsh" "tut-9/index.js"] lines together.

Behind the scenes, Reach generated @exloc["tut-8/build/index.main.sol"] lines of Solidity (which you can look at here: @reachexlink["tut-8/build/index.main.sol"]), 1621 lines of TEAL (which you can look at here: @reachexlink["tut-8/build/index.main.mjs#L584"]), as well as @exloc["tut-8/build/index.main.mjs" -2108] lines of JavaScript (which you can look at here: @reachexlink["tut-8/build/index.main.mjs"]).
If we weren't using Reach, then we'd have to write these @exloc["tut-8/build/index.main.sol" "tut-8/build/index.main.mjs" -482] lines ourselves and ensure that they are consistent and updated at every change to the application.

Now that you've seen an entire Reach application from beginning to end, it's time for you to start working on your own applications!

@itemlist[

@item{You may want to start @seclink["workshop"]{the workshop}, which is a self-study course on practicing and learning Reach through different specific projects.}

@item{Or, maybe you'd like to spend some time in @seclink["guide"]{the guide} learning about the background of some of the concepts used in Reach programs.}

@item{Or, maybe it's time for you to dive into @seclink["ref"]{the reference} and look into the minutae of Reach's features.}

@item{Finally, you may like to repeat a portion of this tutorial, but using @seclink["tut-7-rpc"]{a language other than JavaScript}, like Python or Go!}

]

No matter what you decide to read or work on next, we hope you'll join us on @(the-community-link).
Once you join, message @litchar{@"@"team, I just completed the tutorial!} and we'll give you the @litchar{tutorial veteran} role, so you can more easily help others work through it!

Thanks for spending your afternoon with us!

@include-section["tut-7-rpc.scrbl"]
