#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the hand, if this is too simple, then you may want to look at @seclink["howtos"]{some how-to guides} for larger projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

If you're ready, click through to the first step!

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

Now that your Reach installation is in order, you should open a text editor and get ready to write your first Reach application!

@section[#:tag "tut-1"]{Step 1: Scaffolding and Setup}

In this tutorial, we'll be building a version of @emph{Rock, Paper, Scissors} where two players, @emph{Alice} and @emph{Bob}, can wager on the result of the game.
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

In the next step, we'll implement the logic of @emph{Rock, Paper, Scissors} and our application will start doing something!

@section[#:tag "tut-2"]{Step 2: Rock, Paper, and Scissors}

In this section, we'll have Alice and Bob actually execute the game of @emph{Rock, Paper, Scissors}.

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
(@reachin{(handA + (4 - handB)) % 3} is a clever equation to compute the winner of a game of @emph{Rock, Paper, Scissors} using modular arithmetic.)}

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

Alice is pretty good at @emph{Rock, Paper, Scissors}!

@margin-note{If your version isn't working, look at the complete versions of @reachexlink["tut-2/tut.rsh" @exec{tut.rsh}] and @reachexlink["tut-2/tut.mjs" @exec{tut.mjs}] to make sure you copied everything down correctly!}

Next, we'll add some stakes to the game, because Alice needs to take her skills to the bank!

@section[#:tag "tut-3"]{Step 3: Bets and Wagers}

XXX

@section[#:tag "tut-4"]{Step 4: Trust and Commitments}

XXX

@section[#:tag "tut-5"]{Step 5: Timeouts and Participation}

XXX

@section[#:tag "tut-6"]{Step 6: Play and Play Again}

XXX

@section[#:tag "tut-7"]{Step 7: Interaction and Independence}

XXX

@section[#:tag "tut-8"]{Step 8: Onward and Further}

XXX
