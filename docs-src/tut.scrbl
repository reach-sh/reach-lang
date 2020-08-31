#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

@(define RPS @emph{Rock, Paper, Scissors})

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the other hand, if this is too simple, then you may want to start @seclink["workshop"]{the workshop} for larger and less contrained projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

If you're ready, click through to the @seclink["tut-0"]{first step}!

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "tut-0"]{Install and Initialize}

Reach is designed to work on POSIX systems with @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}. installed.
You'll know that you have them install if you can run

@cmd{docker --version}

and

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

Now that your Reach installation is in order, you should open a text editor and get ready to @seclink["tut-1"]{write your first Reach application}!

@section[#:tag "tut-1"]{Scaffolding and Setup}

In this tutorial, we'll be building a version of @|RPS| where two players, @emph{Alice} and @emph{Bob}, can wager on the result of the game.
We'll start simple and slowly make the application more fully featured.

You should following along by copying each part of the program and seeing how things go.
If you're like us, you may find it beneficial to type each line out, rather than copying & pasting so you can start building your muscle memory and begin to get a sense for each part of a Reach program.

Let's start by creating a file named @exec{index.rsh} and fill it with this:

@reachex[#:show-lines? #t "tut-1/index.rsh"
         #:link #t]

@margin-note{Did you notice that @reachexlink["tut-1/index.rsh"] was a link in the box above the code sample?
You can always click on these links to see the entire file in our @hyperlink["https://github.com/reach-sh/reach-lang"]{GitHub} repository.}

@margin-note{Did your text editor recognize @exec{index.rsh} as a Reach program and give you proper syntax hightlighting?
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
Open a new file named @exec{index.mjs} and fill it with this:

@reachex[#:mode js
         #:show-lines? #t "tut-1/index.mjs"
         #:link #t]

This JavaScript code is similarly schematic and will be consistent across all of your test programs.

@itemlist[

@item{Line 1 imports the Reach standard library.
In this case, we choose Ethereum for simplicity.}

@item{Line 2 imports your backend, which @exec{./reach compile} will produce.}

@item{Line 4 defines an asynchronous function that will be the body of our frontend.}

@item{Line 5 defines a helpful function for defining quantities in ETH.}

@item{Lines 7 and 8 create test accounts with initial endowments for Alice and Bob.
This will only work on the Reach-provided developer testing network.}

@item{Line 10 has Alice deploy the application.}

@item{Line 11 has Bob attach to it.}

@item{Lines 14 through 16 initialize Alice's backend.}

@item{Lines 17 through 19 initialize Bob's backend.}

@item{Line 13 waits for the backends to complete.}

@item{Line 21 calls this asynchronous function that we've defined.}

]

This is now enough for Reach to compile and run our program. Let's try by running

@cmd{./reach run}

Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

@margin-note{The entire process that we just went through can be automated by running @cmd{./reach init} when you start your next project!}

In @seclink["tut-2"]{the next step}, we'll implement the logic of @|RPS| and our application will start doing something!

@section[#:tag "tut-2"]{Rock, Paper, and Scissors}

In this section, we'll have Alice and Bob actually execute the game of @|RPS|.

We have to decide how to represent the hands of the game.
A simple way is to represent them as the numbers @reachin{0}, @reachin{1}, and @reachin{2}, standing for @litchar{Rock}, @litchar{Paper}, and @litchar{Scissors}.
However, Reach only supports unsigned integers of 256 bits, so it is better to represent them as the equivalence class of integers modulo three, so we won't distinguish between @reachin{0} and @reachin{3} as @litchar{Rock}.

We'll use a similar strategy for representing the three outcomes of the game: @litchar{B wins}, @litchar{Draw}, and @litchar{A wins}.

The first step is to change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move that they will play, and later informed of the outcome of the game.

@reachex[#:show-lines? #t "tut-2/index.rsh"
         #:link #t
         'skip 12 25 "      // ..."]

@itemlist[

@item{Lines 3 through 5 defines a @tech{participant interact interface} that will be shared between the two players.
In this case, it provides two methods: @reachin{getHand}, which returns a number; and @reachin{seeOutcome}, which receives a number.}

@item{Line 10 uses this interface for both participants.
Because of this line, @reachin{interact} in the rest of the program will be bound to an object with methods corresponding to the these actions, which will connect to the @tech{frontend} of the corresponding participant.}

]

Before continuing with the Reach application, let's move over to the JavaScript interface and implement these methods in our @tech{frontend}.

@reachex[#:mode js
         #:show-lines? #t "tut-2/index.mjs"
         #:link #t
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

@reachex[#:show-lines? #t "tut-2/index.rsh"
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

@reachex[#:show-lines? #t "tut-2/index.rsh"
         #:link #t
         'only 17 22 "      // ..."]

@itemlist[

@item{Lines 17 through 19 match Alice's similar @tech{local step} and @tech{join}ing of the application through a @tech{consensus transfer} @tech{publication}.}

@item{But, line 21 computes the outcome of the game before committing.
(@reachin{(handA + (4 - handB)) % 3} is a clever equation to compute the winner of a game of @|RPS| using modular arithmetic.
Consider when @reachin{handA} is @reachin{0} (i.e., @litchar{Rock}) and @reachin{handB} is @reachin{2} (i.e., @litchar{Scissors}), then this equation is @reachin{((handA + (4 - handB)) % 3) = ((0 + (4 - 2)) % 3) = ((0 + 2) % 3) = (2 % 3) = 2}, which is the last outcome, that is @litchar{A wins}, as we expect it to be.)}

]

Finally, we use the @tech{each} form to have each of the participants send the final outcome to their frontends.

@reachex[#:show-lines? #t "tut-2/index.rsh"
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

@tech{Consensus networks} in general, and Reach specifically, guarantee that all participants agree on the outcome of the their decentralized computation.
Indeed, this is where the name @tech{consensus network} comes from, as they enable these distributed, and untrusted, parties to come to a consensus, or agreement, about the intermediate states of a computation; and if they agree on the intermediate states, they will also agree on the output.
That's why every time you run @exec{./reach run}, both Alice and Bob will see the same outcome!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-2/index.rsh"] and @reachexlink["tut-2/index.mjs"] to make sure you copied everything down correctly!}

In @seclink["tut-3"]{the next step}, we'll add some stakes to the game, because Alice needs to take her skills to the bank!

@section[#:tag "tut-3"]{Bets and Wagers}

Although it's fun to play @|RPS| with friends for a laugh, it's even better to play it with enemies and your entire life-savings on the line!
Let's change our program so that Alice can offer a wager to Bob and whoever wins will take the pot.

This time, let's start with changes to the JavaScript @tech{frontend} and then we'll go back into the Reach code and connect the new methods up.

Since we're going to be having funds get transfered, we'll record the balances of each participant before the game starts, so we can more clearly show what they won at the end.
We'll add this code in between account creation and contract deployment.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 8 12 "  // ..."]

@itemlist[

@item{Line 10 shows a helpful function for getting the balance of a participant and displaying it.}

@item{Line 11 and 12 get the balance before the game starts for both Alice and Bob.}

]

Next, we'll update Alice's interface object to include her wager.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 27 31 "    // ..."]

@itemlist[

@item{Line 29 splices the common @jsin{Player} interface into Alice's interface.}

@item{Line 30 defines her wager as @litchar{5} units of the @tech{network token}.
This is an example of using a concrete value, rather than a function, in a @tech{participant interact interface}.}

]

For Bob, we'll modify his interface to show the wager and immediately accept it by returning.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 32 36 "    // ..."]

@itemlist[

@item{Line 35 defines the @jsin{acceptWager} function.}

]

Finally, after the computation is over, we'll get the balance again and show a message summarizing the effect.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 37 45 "  // ..."]

@itemlist[

@item{Lines 39 and 40 get the balances afterwards.}

@item{Lines 42 and 43 print out the effect.}

]

These changes to the @tech{frontend} only deal with issues of presentation and interfacing.
The actual business logic of making the wager and transfer the funds will happen in the Reach code.

Let's look at that now.

First, we need to update the @tech{participant interact interface}.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'skip 18 41 "      // ..."]

@itemlist[

@item{Lines 6 through 8 define Alice's interface as the @reachin{Player} interface, plus an integer value called @reachin{wager}.}

@item{Lines 9 through 11 do the same for Bob, where he has a method called @reachin{acceptWager} that can look at the wager value.}

@item{Line 16 associates these interfaces with the corresponding participants.
The format of this line is a @tech{tuple} of @tech{tuples}, where the first value in the @tech{tuple} is a string that names the @tech{backend} @tech{participant} and the second value is the @tech{participant interact interface}.
It's conventional to name them similarly.}

]

Each of the three parts of the application have to be updated to deal with the wager.
Let's look at Alice's first step first.

@reachex[#:show-lines? #t "tut-3/index.rsh"
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

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'only 25 29 "      // ..."]

@itemlist[

@item{Line 26 has Bob accept the wager.
If he doesn't like the terms, his @tech{frontend} can just not respond to this method and the @|DApp| will stall.}

@item{Line 29 has Bob pay the wager as well.}

]

The @|DApp| is now running in a @tech{consensus step}.
Before, it would compute the outcome and then commit the state; but now, it needs to look at the outcome and use it to balance the account.

@reachex[#:show-lines? #t "tut-3/index.rsh"
         #:link #t
         'only 31 38 "      // ..."]

@itemlist[

@item{Lines 33 through 35 computes the amounts given to each participant depending on the outcome.}

@item{Lines 36 and 37 transfer the corresponding amounts.}

@item{Line 38 commits the state of the application and allows the participants to see the outcome and complete.}

]

At this point, we can run the program and see its output by running

@cmd{./reach run}

Since the players act randomly, the results will be different every time.
When I ran the program three times, this is the output I got:

@verbatim{
$ ./reach run
Alice played Paper
Bob accepts the wager of 5.0.
Bob played Rock
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10.0 to 14.999999999999687163.
Bob went from 10.0 to 4.999999999999978229.

$ ./reach run
Alice played Paper
Bob accepts the wager of 5.0.
Bob played Scissors
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10.0 to 4.999999999999687163.
Bob went from 10.0 to 14.999999999999978246.

$ ./reach run
Alice played Rock
Bob accepts the wager of 5.0.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10.0 to 14.999999999999687175.
Bob went from 10.0 to 4.999999999999978229.
}

@margin-note{How come Alice and Bob's balance goes back to @litchar{10.0} each time?
It's because every time we run @exec{./reach run}, it starts a completely fresh instance of the testing network and creates new accounts for each player.}

@margin-note{How come the balances aren't exactly @litchar{10}, @litchar{15}, and @litchar{5}?
It's because Ethereum transactions cost "gas" to run.}

Alice is doing okay, if she keeps this up, she'll make a fortune on @|RPS|!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-3/index.rsh"] and @reachexlink["tut-3/index.mjs"] to make sure you copied everything down correctly!}

Now that there is a reason to play this game, it turns out that there's a major security vulnerability.
We'll fix this in @seclink["tut-4"]{the next step}; make sure you don't launch with this version, or Alice is going to go broke!

@section[#:tag "tut-4"]{Trust and Commitments}

In the last section, we made it so that Alice and Bob can actually exchange currency when they play @|RPS|.
However, the version of the application we wrote has a fundamental flaw: Bob can win every game!

How is that possible?
We showed executions of the game where Alice won, like the following

@verbatim{
$ ./reach run
Alice played Rock
Bob accepts the wager of 5.0.
Bob played Scissors
Alice saw outcome Alice wins
Bob saw outcome Alice wins
Alice went from 10.0 to 14.999999999999687175.
Bob went from 10.0 to 4.999999999999978229.
}

The problem is that these version of the game only executed an @tech{honest} version of Bob, that is, one that followed the Reach program exactly, including in his private @tech{local steps}.
It is possible for a deviant and dis@tech{honest} version of a Bob @tech{backend} to execute different code and always win by computing the appropriate guess based on what value Alice provided for @reachin{handA}.

If we change Bob's code to the following:

@reachex[#:show-lines? #t "tut-4-attack/index.rsh"
         #:link #t
         'only 25 29 "      // ..."]

then he will ignore the @tech{frontend} and just compute the correct value.

If we run this version of the program, we will see output like this:

@verbatim{
$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.0.
Alice saw outcome Bob wins
Bob saw outcome Bob wins
Alice went from 10.0 to 4.999999999999683071.
Bob went from 10.0 to 14.999999999999978232.
}

In this version, unlike the @tech{honest} version, Bob never consults the @tech{frontend} and so it never prints out the message of what hand Bob played.
No matter what Alice chooses, Bob will always win.

@(hrule)

In fact, Reach comes with an    @seclink["guide-assert"]{automatic verification} engine that we can use to mathematically prove that this version will always result in the @reachin{outcome} variable equalling @reachin{0}, which means Bob wins.
We can instruct Reach to prove this theorem by add these lines after computing the @reachin{outcome}:

@reachex[#:show-lines? #t "tut-4-attack/index.rsh"
         #:link #t
         'only 31 34 "      // ..."]

@itemlist[

@item{Line 32 requires that the dis@tech{honest} version of Bob be used for the proof.}

@item{Line 33 conducts the proof by including an @tech{assert} statement in the program.}

]

Before we had this line in the file, when we ran @exec{./reach run}, it would print out the message:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-3/index.txt"
         #:link #t
         'only 2 7 "      // ..."]

But now, it prints out

@reachex[#:mode verbatim
         #:show-lines? #t "tut-4-attack/index.txt"
         #:link #t
         'only 2 7 "      // ..."]

@itemlist[

@item{Line 7 is different and shows that more theorems have been proven about our program.
It prints out three more, rather than one more, because the theorem is proved differently in the different verification modes.}

]

@(hrule)

Many programming languages include @link["https://en.wikipedia.org/wiki/Assertion_(software_development)"]{assertions} like this, but Reach is one of a small category where the compiler doesn't just insert a runtime check for the property, but actually conducts a mathematical proof at compile-time that the expression @emph{always} evaluates to @reachin{true}.

In this case, we used Reach's @seclink["guide-assert"]{automatic verification} engine to prove that an attack did what we expected it would.
But, it is better to use verification to show that @emph{no flaw} exists and @emph{no attack} is possible.

Reach includes some such assertions automatically in every program.
That's why every version of @|RPS| has said that a number of theorems were checked.
We can see what these theorems do by deliberating inserting an error in the program.

Let's change the computation of the payout and make it so that if Alice wins, then she only gets her wager back, not Bob's.

@reachex[#:show-lines? #t "tut-4-attack/index-bad.rsh"
         #:link #t
         'only 34 41 "      // ..."]

@itemlist[

@item{Line 36 has @reachin{[0, 1]}, but should have @reachin{[0, 2]}.}

]

When we run @exec{./reach compile @reachexlink["tut-4-attack/index-bad.rsh"]}, it gives details about the error:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-4-attack/index-bad.txt"
         #:link #t
         'only 4 12 ""]

There's a lot of information in the compiler output that can help an experienced programmer track down the problem. But the most important parts are

@itemlist[

@item{Line 6 says that this is an attempt to prove the theorem that the balance at the end of the program is zero, which means that no @tech{network tokens} are sealed in the @tech{contract} forever.}

@item{Line 7 says that this happens when the program exits on line 45, which directs the programmer to that path through the program.}

]

These kinds of @seclink["guide-assert"]{automatic verifications} are helpful for Reach programmers, because they don't need to remember to put them in their program, and they will still be protected from entire categories of errors.

@(hrule)

However, now let's add an @tech{assert}ion to the program that will ensure that every version of the program that allows Bob to know Alice's hand before he chooses his own will be rejected.

We'll go back to the version of @reachexlink["tut-3/index.rsh"] from the last section, which has an @tech{honest} version of Bob.
(Click on the preceeding link if you need to see what it contained.)

We'll add a single line to the program after Alice publishes, but before Bob takes a @tech{local step}:

@reachex[#:show-lines? #t "tut-4-attack/index-fails.rsh"
         #:link #t
         'only 21 28 "      // ..."]

@itemlist[

@item{Line 25 contains a @tech{knowledge assertion} that Bob cannot know Alice's value @reachin{handA} at this point in the program.
In this case, it is obvious that this is not true, because Alice shares @reachin{handA} at line 21.
In many cases, this is not obvious and Reach's @seclink["guide-assert"]{automatic verification} engine has to reason about how values that Bob @emph{does know} are connected to values that might be related to Alice's secret values.}

]

When we run @exec{./reach run}, it reports that this assertion is false:

@reachex[#:mode verbatim
         #:show-lines? #t "tut-4-attack/index-fails.txt"
         #:link #t
         'only 3 5 ""]

It is not enough to correct failures and attacks when you discover them.
You must @bold{always} add an assertion to your program that would fail to hold if the attack or failure were present.
This ensures that all similar attacks are not present and that they will not accidentally be reintroduced.

@(hrule)

Let's use these insights into @seclink["guide-assert"]{automatic verification} and rewrite our @|RPS| so that it is more trustworthy and secure.

Since we've been making lots of changes to the code, let's start fresh with a new version and we'll look at every single line again, to make sure that you aren't missing anything.

First, we'll define the rules of @|RPS| a little bit more abstractly, so we can separate the logic of the game from the details of the application:

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 1 7 "// ..."]

@itemlist[

@item{Line 1 is the usual Reach version header.}

@item{Lines 3 and 4 define @tech{enumeration}s for the hands that may be played, as well as the outcomes of the game.}

@item{Lines 6 and 7 define the function that computes the winner of the game.}

]

When we first wrote @|RPS|, we asked you to trust that this formula for computing the winner is correct, but is good to actually check.
One way to check would be to implement a JavaScript @tech{frontend} that didn't interact with a real user, nor would it randomly generate values, but instead, it would return specific testing scenario values and check that the output is as expected.
That's a typical way to debug and is possible with Reach.
However, Reach allows us to write such test cases directly into the Reach program as verification assertions.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 9 11 "// ..."]

@itemlist[

@item{Line 9 makes an @tech{assert}ion that when Alice plays Rock and Bob plays Paper, then Bob wins as expected.}

]

But, Reach's @seclink["guide-assert"]{automatic verification} allows us to express even more powerful statements about our program's behavior.
For example, we can state that no matter what values are provided for @reachin{handA} and @reachin{handB}, @reachin{winner} will always provide a valid outcome:

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 13 15 "// ..."]

And we can specify that whenever the same value is provided for both hands, no matter what it is, @reachin{winner} always returns @reachin{DRAW}:

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 17 18 "// ..."]

These examples both use @reachin{forall}, which allows Reach programmers to quantify over all possible values that might be provided to a part of their program.

Let's continue the program by specifying the @tech{participant interact interface}s for Alice and Bob.
These will be mostly the same as before, except that we will also expect that each @tech{frontend} can provide access to random numbers.
We'll use these later on to protect Alice's hand.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 20 35 "// ..."]

The only line that is different is line 21, which includes @reachin{hasRandom}, from the Reach standard library, in the interface.

This is the source of the only line that needs to change in our JavaScript @tech{frontend} as well:

@reachex[#:mode js
         #:show-lines? #t "tut-4/index.mjs"
         #:link #t
         'only 18 24 "  // ..."]

This line of JavaScript allows each @tech{participant}'s Reach code to generate random numbers as necessary.

We're now at the crucial juncture where we will implement the actual application and ensure that Alice's hand is protected until after Bob reveals his hand.
The simplest thing would be to have Alice just publish the wager, but this, of course, would just leave Bob vulnerable.
We need to Alice to be able to publish her hand, but also keep it secret.
This is a job for a @link["https://en.wikipedia.org/wiki/Commitment_scheme"]{cryptographic commitment scheme}.
Reach's standard library comes with @reachin{makeCommitment} to make this easier for you.

@reachex[#:show-lines? #t "tut-4/index.rsh"
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

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 44 50 "      // ..."]

@itemlist[

@item{Line 44 states the @tech{knowledge assertion}.}

@item{Lines 45 through 49 are unchanged from the original version.}

@item{Line 50 has the transaction commit, without computing the payout, because we can't yet, because Alice's hand is not yet public.}

]

We now return to Alice who can reveal her secrets.

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 52 55 "      // ..."]

@itemlist[

@item{Line 53 has Alice declassify the secret information.}

@item{Line 54 has her publish it.}

@item{Line 55 checks that the published values match the original values.
This will always be the case with @tech{honest} participants, but dis@tech{honest} participants may violate this assumption.}

]

The rest of the program is unchanged from the original version, except that it uses the new names for the outcomes:

@reachex[#:show-lines? #t "tut-4/index.rsh"
         #:link #t
         'only 57 68 "      // ..."]

Since we didn't have to change the @tech{frontend} in any meaningful way, the output of running @exec{./reach run} is still the same as it ever was:

@verbatim{
$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.0.
Bob played Paper
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 10.0 to 14.999999999999553643.
Bob went from 10.0 to 4.999999999999969352.

$ ./reach run
Alice played Paper
Bob accepts the wager of 5.0.
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10.0 to 4.999999999999553626.
Bob went from 10.0 to 14.999999999999969352.

$ ./reach run
Alice played Scissors
Bob accepts the wager of 5.0.
Bob played Scissors
Bob saw outcome Draw
Alice saw outcome Draw
Alice went from 10.0 to 9.999999999999550271.
Bob went from 10.0 to 9.999999999999969352.
}

Except now, behind the scenes, and without any changes to the frontend, Alice now takes two steps in program and Bob only takes one, and she is protected against Bob finding her hand and using it to ensure he wins!

When we compile this version of the application, Reach's @seclink["guide-assert"]{automatic formal verification} engine proves many theorems and protects us against a plethora of mistakes one might make when writing even a simple application like this.
Non-Reach programs that try to write decentralized applications are on their own trying to ensure that these problems don't exist.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-4/index.rsh"] and @reachexlink["tut-4/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is secure and doesn't contain any exploits for either Alice or Bob to guarantee a win.
However, it still has a final category of mistake that is common in decentralized applications: @seclink["guide-timeout"]{non-participation}.
We'll fix this in @seclink["tut-5"]{the next step}; make sure you don't launch with this version, or Alice may decide to back out of the game when she knows she's going to lose!

@section[#:tag "tut-5"]{Timeouts and Participation}

In the last section, we removed a security vulnerability from @|RPS| that was a clear attack on the viability of the application.
In this section, we'll focus on a more subtle issue that is important and unique to decentralized applications: @seclink["guide-timeout"]{non-participation}.

Non-participation refers to the act of one party ceasing to continue playing their role in an application.

In traditional client-server programs, like an Web server, this would be the case of a client stopping sending requests to the server, or the server stopping sending responses to the client.
In these sorts of traditional programs, non-participation is an exceptional circumstances that normally leads to an error message for clients and, at most, a log entry for servers.
Sometimes traditional programs will need to recycle resources, like network ports, on non-participation, but they would have also needed to do that if the transaction ended by normal means.
In other words, for traditional client-server programs, it is not necessary for designers to meticulously consider the consequences of non-participation.

In contrast, decentralized applications must be careful designed with an eye towards their behavior in the face of non-participation.
For example, consider what happens in our @|RPS| game if after Alice has paid her wager, Bob never accepts and the application doesn't continue.
In this case, Alice's @tech{network tokens} would be locked inside of the @tech{contract} and lost to her.
Similarly, if after Bob accepted and paid his wager, Alice stopped participating and never submitted her hand, then both their funds would be locked away forever.
In each of these cases, both parties would be greatly hurt and their fear of that outcome would introduce an additional cost to transacting, which would lower the value they got from participating in the application.
Of course, in a situation like @|RPS| this is unlikely to be an important matter, but recall that @|RPS| is a microcosm of decentralized application design.

@margin-note{Technically, in the first case, when Bob fails to start the application, Alice is not locked away from her funds: since Bob's identity is not fixed until after his first message, she could start the game as Bob and then she'd win all of the funds, less any transaction costs of the @tech{consensus network}.
In the second case, however, there would be no recourse for either party.}

In the rest of this section, we'll discuss how Reach helps address non-participation.
For a longer discussion, refer to @seclink["guide-timeout"]{the guide chapter on non-participation}.

@(hrule)

In Reach, non-participation is handled through a "timeout" mechanism whereby each @tech{consensus transfer} can be paired with a @tech{step} that occurs for all @tech{participants} if the @tech{originator} of the @tech{consensus transfer} fails to make the required @tech{publication} before a particular @tech{time}.
We'll integrate this mechanism into our version of @|RPS| and deliberately insert non-participation into our JavaScript testing program to watch the consequences play out.

First, we'll modify the @tech{participant interact interface} to allow the @tech{frontend} to be informed that a timeout occurred.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 20 24 "// ..."]

@itemlist[

@item{Line 24 introduces a new method, @reachin{informTimeout}, that receives no arguments and returns no information.
We'll call this function when a timeout occurs.}

]

We'll make a slight tweak to our JavaScript @tech{frontend} to be able to receive this message and display it on the console.

@reachex[#:mode js
         #:show-lines? #t "tut-5/index.mjs"
         #:link #t
         'only 18 26 "  // ..."]

Back in the Reach program, we'll define an identifier at the top of our program to use a standard deadline throughout the program.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 32 33 "// ..."]

Next, at the start of the Reach application, we'll define a helper function to inform each of the participants of the timeout by calling this new method.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 37 42 "    // ..."]

@itemlist[

@item{Line 38 defines the function as an @tech{arrow expression}.}

@item{Line 39 has each of the participants perform a @tech{local step}.}

@item{Line 40 has them call the new @reachin{informTimeout} method.}

]

We won't change Alice's first message, because there is no consequence to her non-participant: if she doesn't start the game, then no one is any worse off.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 46 47 "      // ..."]

However, we will adjust Bob's first message, because if he fails to participate, then Alice's initial wager will be lost to her.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 54 56 "      // ..."]

@itemlist[

@item{Line 56 adds a timeout handler to Bob's @tech{publication}.}

]

The timeout handler specifies that if Bob does not complete perform this action within a @tech{time delta} of @reachin{DEADLINE}, then the application transitions to @tech{step} given by the arrow expression.
In this case, the timeout code is a call to @reachin{closeTo}, which is a Reach standard library function that has Alice send a message and transfer all of the funds in the @tech{contract} to herself, then call the given function afterwards.
This means that if Bob fails to publish his hand, then Alice will take her @tech{network tokens} back.

We will add a similar timeout handler to Alice's second message.

@reachex[#:show-lines? #t "tut-5/index.rsh"
         #:link #t
         'only 61 62 "      // ..."]

But in this case, Bob will be able to claim all of the funds if Alice doesn't participate.
You might think that it would be "fair" for Alice's funds to be returned to Alice and Bob's to Bob.
However, if we implemented it that way, then Alice would be wise to always timeout if she were going to lose, which she knows will happen, because she knows her hand and Bob's hand.

These are the only changes we need to make to the Reach program to make it robust against non-participation: seven lines!

@(hrule)

Let's modify the JavaScript @tech{frontend} to deliberately cause a timeout sometimes when Bob is supposed to accept the wager.

@reachex[#:mode js
         #:show-lines? #t "tut-5/index.mjs"
         #:link #t
         'only 28 44 "  // ..."]

@itemlist[

@item{Line 37 through 44 redefines Bob's @jsin{acceptWager} method so half of the time it will take at least ten blocks on the Ethereum network by performing ten useless transfer transactions.
We know that ten is the value of @reachin{DEADLINE}, so this will cause a timeout.}

]

@(hrule)

Let's run the program and see what happens:

@verbatim{
$ ./reach run
Alice played Rock
Bob accepts the wager of 5.0.
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10.0 to 4.999999999999386833.
Bob went from 10.0 to 14.999999999999969143.

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
Alice went from 10.0 to 9.999999999999388565.
Bob went from 10.0 to 9.99999999999979.

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
Alice went from 10.0 to 9.999999999999388565.
Bob went from 10.0 to 9.99999999999979.
}

Of course, when you run, you may not get two of the three times ending in a timeout.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-5/index.rsh"] and @reachexlink["tut-5/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is robust against either participant dropping from the game.
In @seclink["tut-6"]{the next step}, we'll extend the application to disallow draws and have Alice and Bob play again until there is a winner.

@section[#:tag "tut-6"]{Play and Play Again}

In this section, we extend our code so application so that Alice and Bob will continue to play the against each other until their game does not end in a draw.

This will only require a change to the Reach program, not the JavaScript @tech{frontend}, but we will take the opportunity to modify the @tech{frontend} so that timeouts can happen to both parties when they are asked to submit their hands.
Let's do that to get it out of the way and not distract from the main task of removing draws.

We'll modify the @jsin{Player} interact object so that it will have a different @jsin{getHand} method, as well take an extra argument, for the player's account.

@reachex[#:mode js
         #:show-lines? #t "tut-6/index.mjs"
         #:link #t
         'only 18 33 "  // ..."]

@itemlist[

@item{Line 18 adds the additional argument.
We'll use this in the new @jsin{getHand} method.}

@item{Lines 23 through 28 moves the forced timeout code that we wrote for Bob's @jsin{acceptWager} function into this method.
We also change the threshold so that timeouts only happen 1% of the time.
This isn't a very interesting behavior, so we'll make it much less frequent.}

@item{Line 27 uses the new parameter so that the delaying player performs needless transfers to and from themselves.}

]

We also adjust the calls to @jsin{Player} in Alice and Bob's initialization code to pass the extra parameter, as well as adjust Bob's @jsin{acceptWager} function to remove the timeout code, since we're testing that differently now.
It's just a matter of reverting to the simpler version from before.

@reachex[#:mode js
         #:show-lines? #t "tut-6/index.mjs"
         #:link #t
         'only 35 46 "  // ..."]

@itemlist[

@item{Line 37 passes the new argument for Alice.}

@item{Line 42 passes it for Bob.}

@item{Line 44 and 45 has the simpler @jsin{acceptWager} method for Bob.}

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

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 42 46 "      // ..."]

@itemlist[

@item{Line 44 has Alice pubish and pay the wager.}

]

@reachex[#:show-lines? #t "tut-6/index.rsh"
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

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 53 55 "      // ..."]

@itemlist[

@item{Line 53 defines the loop variable, @reachin{outcome}.}

@item{Line 54 states the invariant that body of the loop does not change the balance in the @tech{contract} account and that  @reachin{outcome} is a valid outcome.}

@item{Line 55 begins the loop with the condition that it continues as long as the outcome is a draw.}

]

Now, let's look at the body of the loop for the remaining steps, starting with Alice's commitment to her hand.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 56 54 "        // ..."]

@itemlist[

@item{Line 56 commits the last transaction, which at the start of the loop is Bob's acceptance of the wager, and at subsequent runs of the loop is Alice's publication of her hand.}

@item{Lines 58 through 64 are almost identical to the older version, except the wager is already known and paid.}

]

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 66 71 "        // ..."]

Similarly, Bob's code is almost identical to the prior version, except that he's already accepted and paid the wager.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 73 77 "        // ..."]

Alice's next step is actually identical, because she is still revealing her hand in exactly the same way.

Next is the end of the loop.

@reachex[#:show-lines? #t "tut-6/index.rsh"
         #:link #t
         'only 79 80 "        // ..."]

@itemlist[

@item{Line 79 updates the @reachin{outcome} loop variable with the new value.}

@item{Line 80 continues the loop.
Unlike most programming languages, Reach @bold{requires} that @reachin{continue} be explicitly written in the loop body.}

]

The rest of the program could be exactly the same as it was before, except now it occurs outside of the loop, but we will simplify it, because we know that the outcome can never be a draw.

@reachex[#:show-lines? #t "tut-6/index.rsh"
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
Bob accepts the wager of 5.0.
Alice played Paper
Bob played Rock
Bob saw outcome Alice wins
Alice saw outcome Alice wins
Alice went from 10.0 to 14.999999999999040261.
Bob went from 10.0 to 4.999999999999938085.

$ ./reach run
Bob accepts the wager of 5.0.
Alice played Rock
Bob played Rock
Alice played Paper
Bob played Scissors
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10.0 to 4.999999999998975474.
Bob went from 10.0 to 14.999999999999906275.

$ ./reach run
Bob accepts the wager of 5.0.
Alice played Scissors
Bob played Rock
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10.0 to 4.999999999999040265.
Bob went from 10.0 to 14.999999999999938097.
}

As usual, your results may differ, but you should be able to see single round victories like this, as well as multi-round fights and timeouts from either party.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-6/index.rsh"] and @reachexlink["tut-6/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| will always result in a pay-out, which is much more fun for everyone.
In @seclink["tut-7"]{the final step}, we'll show how to exit "testing" mode with Reach and turn our JavaScript into an interactive @|RPS| game with real users.

@section[#:tag "tut-7"]{Interaction and Independence}

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
You can look at them at @reachexlink["tut-7/package.json"] and @reachexlink["tut-7/Dockerfile"], but the details aren't especially important.
However, we'll customize the other two files.

First, let's look at the @reachexlink["tut-7/docker-compose.yml"] file:

@reachex[#:mode yaml
         #:show-lines? #t "tut-7/docker-compose.yml"
         #:link #t]

@itemlist[

@item{Lines 3 through 6 define a service for connecting to a "live" Ethereum network.}

@item{Lines 7 through 13 define a service for running a test instance named @litchar{alice}.}

@item{Lines 14 through 20 duplicate this service with the name @litchar{bob}, so we can start two different instances.}

@item{Lines 21 and 22 define the Reach private developer test network service.}

]

With these inplace, we can run

@cmd{docker-compose run WHICH}

where @exec{WHICH} is @litchar{live} for a live instance, or @litchar{alice} or @litchar{bob} for a test instance.
If we use the live version, then we have to define the environment variable @envvar{ETH_NODE_URI} as the URI of our Ethereum node.

We'll modify the @reachexlink["tut-7/Makefile"] to have commands to run each of these variants:

@reachex[#:mode makefile
         #:show-lines? #t "tut-7/Makefile"
         #:link #t
         'only 15 25 ""]

However, if we try to run either of these, it will do the same thing it always has: create test accounts for each user and simulate a random game.
Let's modify the JavaScript @tech{frontend} and make them interactive.

@(hrule)

We'll start from scratch and show every line of the program again.
You'll see a lot of similarity between this and the last version, but for completeness, we'll show every line.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 1 6 "  // ..."]

@itemlist[

@item{Lines 1 and 2 are the same as before: importing the standard library and the backend.}

@item{Line 3 is new and imports a helpful library for simple console applications called @exec{ask.mjs} from the Reach standard library.
We'll see how these three functions are used below.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 8 12 "  // ..."]

@itemlist[

@item{Lines 8 and 9 ask the question whether they are playing as Alice and expect a "Yes" or "No" answer.
@jsin{ask} presents a prompt and collects a line of input until its argument does not error.
@jsin{yesno} errors if it is not given "y" or "n".}

]

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 14 21 "  // ..."]

@itemlist[

@item{Lines 15 and 16 present the user with the choice of creating a test account if they can or inputing a mnemonic to load an existing account.}

@item{Line 17 creates the test account as before.}

@item{Line 21 loads the existing account.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 23 32 "  // ..."]

@itemlist[

@item{Lines 24 and 25 ask if the participant will deploy the contract.}

@item{Lines 26 and 27 deploy it and print out public information (@jsin{ctc.info}) that can be given to the other player.}

@item{Lines 29 through 27 request, parse, and process this information.
@jsin{stdlib.ctcFromInfo} parses a @jsin{ctc.info} string into a @jsin{ctc} object.}

]

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 34 40 "  // ..."]

Next we define a few helper functions and start the participant interaction interface.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 42 44 "  // ..."]

First we define a timeout handler.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 46 59 "  // ..."]

Next, we request the wager amount or define the @jsin{acceptWager} method, depending on if we are Alice or not.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 61 75 "  // ..."]

Next, we define the shared @jsin{getHand} method.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 77 80 "  // ..."]

Finally, the @jsin{seeOutcome} method.

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 82 89 "  // ..."]

Lastly, we choose the appropriate backend function and await its completion.

@(hrule)

We can now run

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
Do you want to deploy the game? (y/n)
y
The contract is deployed as = {"address": "0xdd1a445d4a85C4676094f84fFe19Fb3d76E502E0", "creation_block": 73}
Your balance is 999.999999999999123878
How much do you want to wager?
10
What hand will you play?
r
You played Rock
The outcome is: Bob wins
Your balance is now 989.999999999999040247}

and

@verbatim{
$ make run-bob
Are you Alice?
n
Starting Rock, Paper, Scissors as Bob
Would you like to create an account? (only possible on devnet)
y
Do you want to deploy the game? (y/n)
n
Please paste the contract information:
{"address": "0xdd1a445d4a85C4676094f84fFe19Fb3d76E502E0", "creation_block": 73}
Your balance is 1000.0
Do you accept the wager of 10.0?
y
What hand will you play?
p
You played Paper
The outcome is: Bob wins
Your balance is now 1009.999999999999938073}

Of course, when you run the exact amounts and addresses may be different.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-7/index.rsh"], @reachexlink["tut-7/index.mjs"], @reachexlink["tut-7/package.json"], @reachexlink["tut-7/Dockerfile"], @reachexlink["tut-7/docker-compose.yml"], and @reachexlink["tut-7/Makefile"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is finished!
We are protected against attacks, timeouts, and draws, and we can run interactively on non-test networks.

In @seclink["tut-8"]{the next section}, we'll summarize where we've gone and direct you to the next step of your journey to decentralized application mastery.

@section[#:tag "tut-8"]{Onward and Further}

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

How difficult was all this?
Let's look at the final versions of our programs.

First, let's look at the Reach program:

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t]

Next, the JavaScript frontend:

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t]

We wrote @exloc["tut-7/index.rsh"] lines of Reach and @exloc["tut-7/index.mjs"] lines of JavaScript, or @exloc["tut-7/index.rsh" "tut-7/index.mjs"] lines together.

Behind the scenes, Reach generated @exloc["tut-7/build/index.main.sol"] lines of Solidity (which you can look at here: @reachexlink["tut-7/build/index.main.sol"]), as well as @exloc["tut-7/build/index.main.mjs" -503] lines of JavaScript (which you can look at here: @reachexlink["tut-7/build/index.main.mjs"]).
If we weren't using Reach, then we'd have to write these @exloc["tut-7/build/index.main.sol" "tut-7/build/index.main.mjs" -503] lines ourselves and ensure that they are consistent and updated at every change to the application.

Now that you've seen an entire Reach application from beginning to end, it's time for you to start working on your own application!

@itemlist[

@item{You may want to start @seclink["workshop"]{the workshop}, which is a self-study course on practicing and learning Reach through different specific projects.}

@item{Or, maybe you'd like to spent some time in @seclink["guide"]{the guide} learning about the background of some of the concepts used in Reach programs.}

@item{Or, maybe it's time for you to dive into @seclink["ref"]{the reference} and look into the minutae of Reach's features.}

]

Now matter what you decide to read or work on next, we hope you'll join us on @(the-community-link).
Once you join, message @litchar{@"@"team, I just completed the tutorial!} and we'll give you the @litchar{tutorial veteran} role, so you can more easily help others work through it!

Thanks for spending your afternoon with us!
