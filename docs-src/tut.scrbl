#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

@(define RPS @emph{Rock, Paper, Scissors})

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the hand, if this is too simple, then you may want to look at @seclink["howtos"]{some how-to guides} for larger projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

If you're ready, click through to the @seclink["tut-0"]{first step}!

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "tut-0"]{Step 0: Install and Initialize}

Reach is designed to work on POSIX systems with @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}. installed.
You'll know that you have them install if you can run

@cmd{docker --version}

and

@cmd{docker-compose --version}

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

Now that your Reach installation is in order, you should open a text editor and get ready to @seclink["tut-1"]{write your first Reach application}!

@section[#:tag "tut-1"]{Step 1: Scaffolding and Setup}

In this tutorial, we'll be building a version of @|RPS| where two players, @emph{Alice} and @emph{Bob}, can wager on the result of the game.
We'll start simple and slowly make the application more fully featured.

You should following along by copying each part of the program and seeing how things go.
If you're like us, you may find it beneficial to type each line out, rather than copying & pasting so you can start building your muscle memory and begin to get a sense for each part of a Reach program.

Let's start by creating a file named @exec{tut.rsh} and fill it with this:

@reachex[#:show-lines? #t "tut-1/tut.rsh"
         #:link "tut.rsh"]

@margin-note{Did you notice that @reachexlink["tut-1.rsh" @exec{tut.rsh}] was a link in the box above the code sample?
You can always click on these links to see the entire file in our @hyperlink["https://github.com/reach-sh/reach-lang"]{GitHub} repository.}

@margin-note{Did your text editor recognize @exec{tut.rsh} as a Reach program and give you proper syntax hightlighting?
If not, then you can manually configure it to treat Reach (@exec{.rsh}) files as JavaScript and things will be mostly correct.}

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
Open a new file named @exec{tut.mjs} and fill it with this:

@reachex[#:mode js
         #:show-lines? #t "tut-1/tut.mjs"
         #:link "tut.mjs"]

This JavaScript code is similarly schematic and will be consistent across all of your test programs.

@itemlist[

@item{Line 1 imports the Reach standard library.
In this case, we choose Ethereum for simplicity.}

@item{Line 2 imports your backend, which @exec{reach compile} will produce.}

@item{Line 5 defines a helpful function for defining quantities in ETH.}

@item{Lines 7 and 8 create test accounts with initial endowments for Alice and Bob.
This will only work on the Reach-provided developer testing network.}

@item{Line 10 has Alice deploy the application.}

@item{Line 11 has Bob attach to it.}

@item{Lines 14 through 16 initialize Alice's backend.}

@item{Lines 17 through 19 initialize Bob's backend.}

@item{Line 13 waits for the backends to complete.}

]

This is now enough for Reach to compile and run our program. Let's try by running

@cmd{reach run tut}

Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

In @seclink["tut-2"]{the next step}, we'll implement the logic of @|RPS| and our application will start doing something!

@section[#:tag "tut-2"]{Step 2: Rock, Paper, and Scissors}

In this section, we'll have Alice and Bob actually execute the game of @|RPS|.

We have to decide how to represent the hands of the game.
A simple way is to represent them as the numbers @reachin{0}, @reachin{1}, and @reachin{2}, standing for @litchar{Rock}, @litchar{Paper}, and @litchar{Scissors}.
However, Reach only supports unsigned integers of 256 bits, so it is better to represent them as the equivalence class of integers modulo three, so we won't distinguish between @reachin{0} and @reachin{3} as @litchar{Rock}.

We'll use a similar strategy for representing the three outcomes of the game: @litchar{B wins}, @litchar{Draw}, and @litchar{A wins}.

The first step is to change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move that they will play, and later informed of the outcome of the game.

@reachex[#:show-lines? #t "tut-2/tut.rsh"
         #:link "tut.rsh"
         'skip 12 25 "      // ..."]

@itemlist[

@item{Lines 3 through 5 defines a @tech{participant interact interface} that will be shared between the two players.
In this case, it provides two methods: @reachin{getHand}, which returns a number; and @reachin{seeOutcome}, which receives a number.}

@item{Line 10 uses this interface for both participants.
Because of this line, @reachin{interact} in the rest of the program will be bound to an object with methods corresponding to the these actions, which will connect to the @tech{frontend} of the corresponding participant.}

]

Before continuing with the Reach application, let's move over to the JavaScript interface and implement these methods in our @tech{frontend}.

@reachex[#:mode js
         #:show-lines? #t "tut-2/tut.mjs"
         #:link "tut.mjs"
         'only 13 29 "  // ..."]

@itemlist[

@item{Lines 13 and 14 define arrays to hold the meaning of the hands and outcomes.}

@item{Line 15 defines a constructor for the @jsin{Player} implementation.}

@item{Lines 16 through 18 define the @jsin{getHand} method.}

@item{Lines 19 and 20 define the @jsin{seeOutcome} method.}

@item{Finally, lines 25 and 28 instantiate the implementation once for Alice and once for Bob.
These are the actual objects that will be bound to @reachin{interact} in the Reach program.}

]

There should be nothing interesting or controversial about these implementations; that's the point of Reach: we get to just write normal business logic without worrying about the details of the @tech{consensus network} and decentralized application.

Let's return to the Reach program and look inside of the body of the program for what actions Alice and Bob take.

The game proceeds in three steps.

First, Alice's backend interacts with her frontend, gets her hand, and publishes it.

@reachex[#:show-lines? #t "tut-2/tut.rsh"
         #:link "tut.rsh"
         'only 12 15 "      // ..."]

@itemlist[

@item{Line 12 states that this block of code is something that @emph{only} @reachin{A} (i.e., Alice) performs.}

@item{That means that the variable, @reachin{handA}, bound on line 13 is known only to Alice.}

@item{Line 13 also @tech{declassifies} the value, because in Reach, all information from @tech{frontends} is @tech{secret} until it is explicitly made public.}

@item{Line 14 publishes the value to the @tech{consensus network}, so it can be used to evaluate the outcome of the game.
Once this happens, the code is in a "@tech{consensus step}" where all participants act together.}

@item{Line 15 commits the state of the @tech{consensus network} and returns to "@tech{local step}" where individual participants can act alone.}

]

The next step is similar, in that Bob publishes his hand; however, we don't immediately commit the state, instead we compute the outcome of the game.

@reachex[#:show-lines? #t "tut-2/tut.rsh"
         #:link "tut.rsh"
         'only 17 22 "      // ..."]

@itemlist[

@item{Lines 17 through 19 match Alice's similar @tech{local step} and @tech{consensus transfer}.}

@item{But, line 21 computes the outcome of the game before committing.
(@reachin{(handA + (4 - handB)) % 3} is a clever equation to compute the winner of a game of @|RPS| using modular arithmetic.)}

]

Finally, we use the @tech{each} form to have each of the participants send the final outcome to their frontends.

@reachex[#:show-lines? #t "tut-2/tut.rsh"
         #:link "tut.rsh"
         'only 24 25 "      // ..."]

@itemlist[

@item{Line 24 states that this is a @tech{local step} that @tech{each} of the participants performs.}

]

At this point, we can run the program and see its output by running

@cmd{reach run tut}

Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

@verbatim{
$ reach run tut
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ reach run tut
Alice played Scissors
Bob played Paper
Alice saw outcome Alice wins
Bob saw outcome Alice wins

$ reach run tut
Alice played Paper
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
}

Alice is pretty good at @|RPS|!

@tech{Consensus networks} in general, and Reach specifically, guarantee that all participants agree on the outcome of the their decentralized computation.
Indeed, this is where the name @tech{consensus network} comes from, as they enable these distributed, and untrusted, parties to come to a consensus, or agreement, about the intermediate states of a computation; and if they agree on the intermediate states, they will also agree on the output.
That's why every time you run @exec{reach run tut}, both Alice and Bob will see the same outcome!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-2/tut.rsh" @exec{tut.rsh}] and @reachexlink["tut-2/tut.mjs" @exec{tut.mjs}] to make sure you copied everything down correctly!}

In @seclink["tut-3"]{the next step}, we'll add some stakes to the game, because Alice needs to take her skills to the bank!

@section[#:tag "tut-3"]{Step 3: Bets and Wagers}

Although it's fun to play @|RPS| with friends for a laugh, it's even better to play it with enemies and your entire life-savings on the line!
Let's change our program so that Alice can offer a wager to Bob and whoever wins will take the pot.

This time, let's start with changes to the JavaScript @tech{frontend} and then we'll go back into the Reach code and connect the new methods up.

Since we're going to be having funds get transfered, we'll record the balances of each participant before the game starts, so we can more clearly show what they won at the end.
We'll add this code in between account creation and contract deployment.

@reachex[#:mode js
         #:show-lines? #t "tut-3/tut.mjs"
         #:link "tut.mjs"
         'only 7 13 "  // ..."]

@itemlist[

@item{Line 9 shows a helpful function for getting the balance of a participant and displaying it.}

@item{Line 10 and 11 get the balance before the game starts for both Alice and Bob.}

]

Next, we'll update Alice's interface object to include her wager.

@reachex[#:mode js
         #:show-lines? #t "tut-3/tut.mjs"
         #:link "tut.mjs"
         'only 26 30 "    // ..."]

@itemlist[

@item{Line 28 splices the common @jsin{Player} interface into Alice's interface.}

@item{Line 29 defines her wager as @litchar{5} units of the @tech{network token}.
This is an example of using a concrete value, rather than a function, in a @tech{participant interact interface}.}

]

For Bob, we'll modify his interface to show the wager and immediately accept it by returning.

@reachex[#:mode js
         #:show-lines? #t "tut-3/tut.mjs"
         #:link "tut.mjs"
         'only 31 35 "    // ..."]

@itemlist[

@item{Line 34 defines the @jsin{acceptWager} function.}

]

Finally, after the computation is over, we'll get the balance again and show a message summarizing the effect.

@reachex[#:mode js
         #:show-lines? #t "tut-3/tut.mjs"
         #:link "tut.mjs"
         'only 36 44 "  // ..."]

@itemlist[

@item{Lines 38 and 39 get the balances afterwards.}

@item{Lines 41 and 42 print out the effect.}

]

These changes to the @tech{frontend} only deal with issues of presentation and interfacing.
The actual business logic of making the wager and transfer the funds will happen in the Reach code.

Let's look at that now.

First, we need to update the @tech{participant interact interface}.

@reachex[#:show-lines? #t "tut-3/tut.rsh"
         #:link "tut.rsh"
         'skip 18 41 "     // ..."]

@itemlist[

@item{Lines 6 through 8 define Alice's interface as the @reachin{Player} interface, plus an integer value called @reachin{wager}.}

@item{Lines 9 through 11 do the same for Bob, where he has a method called @reachin{acceptWager} that can look at the wager value.}

@item{Line 16 associates these interfaces with the corresponding participants.
The format of this line is a @tech{tuple} of @tech{tuples}, where the first value in the @tech{tuple} is a string that names the @tech{backend} @tech{participant} and the second value is the @tech{participant interact interface}.
It's conventional to name them similarly.}

]

Each of the three parts of the application have to be updated to deal with the wager.
Let's look at Alice's first step first.

@reachex[#:show-lines? #t "tut-3/tut.rsh"
         #:link "tut.rsh"
         'only 18 23 "     // ..."]

@itemlist[

@item{Line 19 has Alice @tech{declassify} the wager for transmission.}

@item{Line 21 is updated so that Alice shares the wager amount with Bob.}

@item{Line 22 has her transfer the amount as part of her @tech{publication}.
The Reach compiler would throw an exception if @reachin{wager} did not appear on line 21, but did appear on line 22.
Change the program and try it.
This is because the @tech{consensus network} needs to be able to verify that the amount of @tech{network tokens} included in Alice's @tech{publication} match some computation available to @tech{consensus network}.}

]

Next, Bob needs to be shown the wager and given the opportunity to accept it and transfer his funds.

@reachex[#:show-lines? #t "tut-3/tut.rsh"
         #:link "tut.rsh"
         'only 25 29 "     // ..."]

@itemlist[

@item{Line 26 has Bob accept the wager.
If he doesn't like the terms, his @tech{frontend} can just not respond to this method and the @|DApp| will stall.}

@item{Line 29 has Bob pay the wager as well.}

]

The @|DApp| is now running in a @tech{consensus step}.
Before, it would compute the outcome and then commit the state; but now, it needs to look at the outcome and use it to balance the account.

@reachex[#:show-lines? #t "tut-3/tut.rsh"
         #:link "tut.rsh"
         'only 31 38 "     // ..."]

@itemlist[

@item{Lines 33 through 35 computes the amounts given to each participant depending on the outcome.}

@item{Lines 36 and 37 transfer the corresponding amounts.}

@item{Line 38 commits the state of the application and allows the participants to see the outcome and complete.}

]

At this point, we can run the program and see its output by running

@cmd{reach run tut}

Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

@verbatim{
$ reach run tut
Alice played Paper
Bob accepts the wager of 5.0.
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10.0 to 14.999999999999687163.
Bob went from 10.0 to 4.999999999999978229.

$ reach run tut
Alice played Paper
Bob accepts the wager of 5.0.
Bob played Scissors
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10.0 to 4.999999999999687163.
Bob went from 10.0 to 14.999999999999978246.

$ reach run tut
Alice played Rock
Bob accepts the wager of 5.0.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10.0 to 14.999999999999687175.
Bob went from 10.0 to 4.999999999999978229.
}

@margin-note{How come Alice and Bob's balance goes back to @litchar{10.0} each time?
It's because every time we run @exec{reach run tut}, it starts a completely fresh instance of the testing network and creates new accounts for each player.}

Alice is doing okay, if she keeps this up, she'll make a fortune on @|RPS|!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-3/tut.rsh" @exec{tut.rsh}] and @reachexlink["tut-3/tut.mjs" @exec{tut.mjs}] to make sure you copied everything down correctly!}

Now that there is a reason to play this game, it turns out that there's a major security vulnerability.
We'll fix this in @seclink["tut-4"]{the next step}; make sure you don't launch with this version, or Alice is going to go broke!

@section[#:tag "tut-4"]{Step 4: Trust and Commitments}

XXX

@section[#:tag "tut-5"]{Step 5: Timeouts and Participation}

XXX

@section[#:tag "tut-6"]{Step 6: Play and Play Again}

XXX

@section[#:tag "tut-7"]{Step 7: Interaction and Independence}

XXX

@section[#:tag "tut-8"]{Step 8: Onward and Further}

Let's review what we've done through this tutorial:

@itemlist[

@item{In @seclink["tut-0"]{part zero}, we saw how Reach can be installed with one command on almost any system without any dependencies beyond what most developers have anyways.}

@item{In @seclink["tut-1"]{part one}, we saw how Reach programs have a succinct setup that easily abstracts the details of your chosen @tech{consensus network} into a couple lines and three key API calls.}

@item{In @seclink["tut-2"]{part two}, we saw how Reach allows developers to focus on the business logic of their decentralized application and look past the nitty-gritty details of blockchain interaction and protocol design.}

@item{In @seclink["tut-3"]{part three}, we saw that it is just as easy for Reach to deal with tokens and network transactions as it is to deal with data sharing.}

@item{In @seclink["tut-4"]{part four}, we were introduce to the Reach @seclink["guide-assert"]{automatic formal verification} engine and its ability to ensure our program doesn't have entire categories of flaws and security vulnerabilities.}

@item{In @seclink["tut-5"]{part five}, we saw how Reach allows you to specify how to deal with @seclink["guide-timeout"]{non-participation} and protect against funds being locked in contracts.}

@item{In @seclink["tut-6"]{part six}, we saw how Reach can express arbitrary length interactions and how flexible the Reach @tech{frontends} are to variations in the @tech{backend}.}

@item{In @seclink["tut-7"]{part seven}, we saw how to decouple your Reach program from the Reach standard testing environment and launch an interactive version on a real network.}

]

Despite having done so much, this is really just a brief introduction to what is possible with Reach.

It's now time for you to start working on your own application!

@itemlist[

@item{You may want to continue reading with a @seclink["howtos"]{how-to guide} that walks through the development of a different application.}

@item{Or, maybe you'd like to spent some time in @seclink["guide"]{the guide} learning about the background of some of the concepts used in Reach programs.}

@item{Or, maybe it's time for you to dive into @seclink["ref"]{the reference} and look into the minutae of Reach's features.}

]

Now matter what you decide to read or work on next, we hope you'll join us on @(the-community-link).
Once you join, message @litchar{@"@"team, I just completed the tutorial!} and we'll give you the @litchar{tutorial veteran} role, so you can more easily help others work through it!

Thanks for spending your afternoon with us!

XXX show programs one last time
