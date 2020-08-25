#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

@(define RPS @emph{Rock, Paper, Scissors})

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the hand, if this is too simple, then you may want to start @seclink["workshop"]{the workshop} for larger and less contrained projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

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

@cmd{./reach run}

Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

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

@item{Line 13 also @tech{declassifies} the value, because in Reach, all information from @tech{frontends} is @tech{secret} until it is explicitly made public.}

@item{Line 14 publishes the value to the @tech{consensus network}, so it can be used to evaluate the outcome of the game.
Once this happens, the code is in a "@tech{consensus step}" where all participants act together.}

@item{Line 15 commits the state of the @tech{consensus network} and returns to "@tech{local step}" where individual participants can act alone.}

]

The next step is similar, in that Bob publishes his hand; however, we don't immediately commit the state, instead we compute the outcome of the game.

@reachex[#:show-lines? #t "tut-2/index.rsh"
         #:link #t
         'only 17 22 "      // ..."]

@itemlist[

@item{Lines 17 through 19 match Alice's similar @tech{local step} and @tech{consensus transfer}.}

@item{But, line 21 computes the outcome of the game before committing.
(@reachin{(handA + (4 - handB)) % 3} is a clever equation to compute the winner of a game of @|RPS| using modular arithmetic.)}

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
         'only 7 13 "  // ..."]

@itemlist[

@item{Line 9 shows a helpful function for getting the balance of a participant and displaying it.}

@item{Line 10 and 11 get the balance before the game starts for both Alice and Bob.}

]

Next, we'll update Alice's interface object to include her wager.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 26 30 "    // ..."]

@itemlist[

@item{Line 28 splices the common @jsin{Player} interface into Alice's interface.}

@item{Line 29 defines her wager as @litchar{5} units of the @tech{network token}.
This is an example of using a concrete value, rather than a function, in a @tech{participant interact interface}.}

]

For Bob, we'll modify his interface to show the wager and immediately accept it by returning.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 31 35 "    // ..."]

@itemlist[

@item{Line 34 defines the @jsin{acceptWager} function.}

]

Finally, after the computation is over, we'll get the balance again and show a message summarizing the effect.

@reachex[#:mode js
         #:show-lines? #t "tut-3/index.mjs"
         #:link #t
         'only 36 44 "  // ..."]

@itemlist[

@item{Lines 38 and 39 get the balances afterwards.}

@item{Lines 41 and 42 print out the effect.}

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

In fact, Reach comes with an @seclink["guide-assert"]{automatic verification} engine that we can use to mathematically prove that this version will always result in the @reachin{outcome} variable equalling @reachin{0}, which means Bob wins.
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

Except now, behind the scenes, and unbeknownest to the business logic, Alice now takes two steps in program and Bob only takes one, and she is protected against Bob finding her hand and using it to ensure he wins!

When we compile this version of the application, Reach's @seclink["guide-assert"]{automatic formal verification} engine proves many theorems and protects us against a plethora of mistakes one might make when writing even a simple application like this.
Non-Reach programs that try to write decentralized applications are on their own trying to ensure that these problems don't exist.

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-4/index.rsh"] and @reachexlink["tut-4/index.mjs"] to make sure you copied everything down correctly!}

Now our implementation of @|RPS| is secure and doesn't contain any exploits for either Alice or Bob to guarantee a win.
However, it still has a final category of mistake that is common in decentralized applications: @seclink["guide-timeout"]{non-participation}.
We'll fix this in @seclink["tut-5"]{the next step}; make sure you don't launch with this version, or Alice may decide to back out of the game when she knows she's going to lose!

@section[#:tag "tut-5"]{Timeouts and Participation}

XXX

@section[#:tag "tut-6"]{Play and Play Again}

XXX

@section[#:tag "tut-7"]{Interaction and Independence}

XXX

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

It's now time for you to start working on your own application!

@itemlist[

@item{You may want to start @seclink["workshop"]{the workshop}, which is a self-study course on practicing and learning Reach through different specific projects.}

@item{Or, maybe you'd like to spent some time in @seclink["guide"]{the guide} learning about the background of some of the concepts used in Reach programs.}

@item{Or, maybe it's time for you to dive into @seclink["ref"]{the reference} and look into the minutae of Reach's features.}

]

Now matter what you decide to read or work on next, we hope you'll join us on @(the-community-link).
Once you join, message @litchar{@"@"team, I just completed the tutorial!} and we'll give you the @litchar{tutorial veteran} role, so you can more easily help others work through it!

Thanks for spending your afternoon with us!

XXX show programs one last time
