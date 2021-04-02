#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "top" #:style 'toc]{Pay-To-Play Tic-Tac-Toe Tutorial}
@author[(author+email "squidKid-deluxe" "squid_kid@tutamail.com")]


This tutorial walks through the creation of a Pay-To-Play Tic-Tac-Toe decentralized application using Reach.
Pay-To-Play Tic-Tac-Toe is a game very similar to regular Tic-Tac-Toe, except that each place on the board has a fee that is calculated by the number of 'wins' attainable in the chosen space.
This workshop includes everything you need to know to build and test this application. It assumes no prior experience with blockchain development.

To dive in, click to go to the @seclink["tic-tac-0"]{first step}!

@local-table-of-contents[#:style 'immediate-only]


@section[#:tag "tic-tac-0"]{Install and Initialize}
@(mint-scope "tic-tac-0")

Reach is designed to work on POSIX systems with @link["https://en.wikipedia.org/wiki/Make_(software)"]{Make}, @link["https://www.docker.com/get-started"]{Docker}, and @link["https://docs.docker.com/compose/install/"]{Docker Compose} installed.
The best way to install Docker on Mac and Windows is with @link["https://www.docker.com/products/docker-desktop"]{Docker Desktop}.

@margin-note{You probably already have @exec{make} installed.
For example, OS X and many other POSIX systems come with @exec{make}, but some versions of Linux do not include it by default and will require you to install it.
If you're on Ubuntu, you can run @exec{sudo apt install make} to get it.

You'll know that you have everything installed if you can run the following three commands without errors:

@cmd{make --version}

@cmd{docker --version}

@cmd{docker-compose --version}

Once you've confirmed that they are installed, choose a directory for this project. We recommend @cmd{mkdir -p ~/reach/tic-tac && cd ~/reach/tic-tac}.

Next, install Reach by downloading it from @hyperlink["https://GitHub.com/reach-sh/reach-lang"]{GitHub} by running @cmd{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach}.

You'll know that the download worked if you can run @cmd{./reach version}.

Since Reach is Dockerized, when you first use it, you'll need to download the images it uses.
This will happen automatically when you first use it, but you can do it manually now by running @cmd{./reach update}.

You'll know that everything is in order if you can run @cmd{./reach compile --help}.


Now that your Reach installation is in order, you should open a text editor and get ready to @seclink["tic-tac-1"]{write your first Reach application}!

@seclink["top"]{Main menu}


@section[#:tag "tic-tac-1"]{Scaffolding and Setup}
@(mint-scope "tic-tac-1")

In this tutorial, we'll be building a version of Tic-Tac-Toe where two players, @emph{Alice} and @emph{Bob}, can wager on the result of the game.
We'll start simple and slowly make the application more full-featured.

You should follow along by copying each part of the program and seeing how things go.
You may find it beneficial to type each line out, rather than copying and pasting so you can start building your muscle memory and begin to get a sense for each part of a Reach program.

Let's start by creating a file named @exec{index.rsh}.
It doesn't matter where you put this file, but we recommend putting in the current directory, which would be @exec{~/reach/tic-tac} if you're following along exactly.
In all the subsequent code samples, we'll label the files based on the chapter of the tutorial you're reading.
For example, start off by typing the following into @exec{index.rsh}:


@reachex[#:show-lines? #t "tic-tac-1/index.rsh"
         #:link #t]


This is just a shell of a program that doesn't do much, but it has a few important components.

@itemlist[

@item{Line 1 indicates that this is a Reach program.
You'll always have this at the top of every program.}

@item{Line 3 defines the main export from the program.
When you compile, this is what the compiler will look at.}

@item{Line 6 specifies the two participants to this application, @emph{Alice} and @emph{Bob}.}

]

Before we go too much further, let's create a similar shell for our JavaScript @tech{frontend} code.
Open a new file named @exec{index.mjs} and fill it with this:

@reachex[#:mode js
         #:show-lines? #t "tic-tac-1/index.mjs"
         #:link #t]


This JavaScript code is similarly schematic and will be consistent across all of your test programs.

@itemlist[

@item{Line 1 imports the Reach standard library loader.}

@item{Line 2 imports your backend, which @exec{./reach compile} will produce.}

@item{Line 4 defines an asynchronous function that will be the body of our frontend.}

@item{Line 5 loads the standard library dynamically based on the @envvar{REACH_CONNECTOR_MODE} environment variable.}

@item{Line 6 defines a quantity of @tech{network token}s as the starting balance for each test account.}

@item{Lines 8 and 9 create test accounts with initial endowments for Alice and Bob.
This will only work on the Reach-provided developer testing network.}

@item{Line 11 has Alice deploy the application.}

@item{Line 12 has Bob attach to it.}

@item{Lines 15 through 18 initialize Alice's backend.}

@item{Lines 19 through 22 initialize Bob's backend.}

@item{Line 14 waits for the backends to complete.}

@item{Line 24 calls this asynchronous function that we've defined.}

]

This is now enough for Reach to compile and run our program. Let's try by running @cmd{./reach run}.

Reach should now build and launch a Docker container for this application.
Since the application doesn't do anything, you'll just see a lot of diagnostic messages though, so that's not very exciting.

@margin-note{The entire process that we just went through can be automated by running @exec{./reach init} when you start your next project!}

In @seclink["tic-tac-2"]{the next step}, we'll implement the logic of Tic-Tac-Toe and our application will start doing something!

@seclink["top"]{Main menu}

@section[#:tag "tic-tac-2"]{Tic-Tac-Toe}
@(mint-scope "tic-tac-2")

In this section, we'll have Alice and Bob actually execute the game of Tic-Tac-Toe.

Alice will always be X and Bob will always be O.

We have to decide how to represent the hands of the game.

The first step is to add the global variables for the Tic-Tac-Toe implementation in the Reach program.
First, we'll add the board variables.

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 4 15 "  // ..."]

@itemlist[

@item{Line 5 defines the @litchar{Board} template.}

@item{Line 4 defines the @litchar{State} template.}

@item{Line 10 actually makes the board @litchar{Array}, or matrix.}

@item{Line 12 returns the finite state machine of the game, including two boards and whose turn it is.}

]

Next, we'll add a few of the winning functions.


@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 17 48 "  // ..."]

@itemlist[

@item{Line 17 defines the @litchar{occupied_cell} function, which finds out if a given place on the board is occupied.}

@item{Line 19 defines the @litchar{occupied_matrix} function, which returns a matrix of all of the occupied places.}

@item{Lines 21-34 check if there is a winner, or a three-in-a-row in any possible direction.}

@item{The function on line 37 returns true if there is a winner on the given board.}

@item{The function on line 39 returns true if X won.}

@item{The function on line 40 returns true if O won.}

@item{The function on line 43 returns true if the board is full.}

@item{The function on lines 45-48 returns true if the game is over because someone won or a draw occurred.}

]

Once that is done, we'll add the functions used for making moves.


@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 50 65 "  // ..."]

@itemlist[

@item{Line 50 defines the @litchar{is_legal} function, which makes sure that a given place on the board is within the boundaries of the board.}

@item{Line 51 defines the @litchar{is_valid} function, which returns true if the given move is not occupied.}

@item{Lines 53-57 get a move from the JS, check if it is valid and legal, and return it.}

@item{Lines 59-65 return an updated state if the given move was valid and legal.}

]

Now, we will change the Reach program to specify that Alice and Bob's frontends can be interacted with to get the move that they will play, and later informed of the outcome of the game.

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 68 84"      // ..."]

@itemlist[

@item{Lines 69 through 78 define @tech{participant interact interfaces} that the two players will use.}

@item{Line 83 uses this interface for both participants.}

]

Before continuing with the Reach application, let's move over to the JavaScript interface and add the @litchar{render} function.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-2/index.mjs"
         #:link #t
         'only 6 23 "  // ..."]

@itemlist[

@item{Line 8 adds the top of the board to the board text.}

@item{Line 9 sets up a 0-8 for loop. @itemlist[

@item{If the current item is an x, line 10 adds an X to the board text.}

@item{If the current item is an o, line 12 adds an O to the board text.}

@item{Otherwise, line 15 adds whitespace to the board text.}

@item{If the row is complete, line 18 adds a divider to the board text.}
]}

@item{Line 21 adds the bottom of the board to the board text.}

@item{Line 22 returns the board text.}

]

Then, let's add some wagering-related variables to our frontend.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-2/index.mjs"
         #:link #t
         'only 35 39 "  // ..."]

@itemlist[

@item{Line 35 creates the wager amount variable.}

@item{Line 36 defines a function to get the balance of a player.}

@item{Line 37-38 find the balance of both players.}

@item{Line 12 defines a function to pretty-print balances.}

]

Next, let's implement the methods described in our backend in the @tech{frontend}.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-2/index.mjs"
         #:link #t
         'only 41 65 "  // ..."]

@itemlist[

@item{Line 47 defines the @litchar{interactWith} function.}

@item{Line 48 allows the reach program to use random.}

@item{Lines 49-52 define the @litchar{getWager} function that returns the wager to the reach program.}

@item{Lines 53-55 define a function to allow Bob to accecept the wager.}

@item{Lines 56-70 define the @litchar{getMove} function:

@itemlist[

@item{Line 57 prints a message that the player has chosen a move.}

@item{Lines 58 and 59 localize the state.xs and state.os variables.}

@item{Line 60 sets up a while loop.}

@item{Line 61 gets a random move from 0-8 and stores it to @litchar{i}.}

@item{Lines 62-63 check if the move is valid (not occupied).}

@item{If the move is valid, line 63 returns the move.}

]}

@item{Line 68 defines the @litchar{endsWith} function.}

@item{Line 69 prints the end state.}

]

After that, modify the interaction promises from line 66-75 to include @litchar{interactWith}.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-2/index.mjs"
         #:link #t
         'only 67 70 "  // ..."]

To finish off the JavaScript frontend, we will add a few @litchar{console.log()} statements to the end of the program.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-2/index.mjs"
         #:link #t
         'only 72 75 "  // ..."]

There should be nothing interesting or controversial about these implementations; that's the point of Reach: we get to just write normal business logic without worrying about the details of the @tech{consensus network} and decentralized application.

Now, let's go back to the Reach program and look inside of the body of the program for what actions Alice and Bob take.

First, both parties get a random number from 0-1 from the JavaScript:

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 85 104 "      // ..."]

Then, the Reach script calculates the player to go first:

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 105 105"      // ..."]

The game proceeds in two ongoing steps.

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 107 127 "      // ..."]

@itemlist[

@item{While the game has not ended because the board is full or someone won: @itemlist[

@item{If it is Alice's turn, Alice's backend interacts with her frontend, gets her hand, publishes it, and applies it to the board.}

@item{Otherwise, Bob's backend interacts with his frontend, gets his hand, publishes it, and applies it to the board.}
]}]

Then, once the game is over, we compute the outcome of the game and pay each player.

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 129 135 "      // ..."]

@itemlist[

@item{Lines 129 through 132 compute the pot for each party.}

@item{Lines 133 through 134 pay each player.}

]

Finally, we use the @tech{each} form to have each of the participants send the final outcome to their frontends.

@reachex[#:show-lines? #t "tic-tac-2/index.rsh"
         #:link #t
         'only 137 138 "      // ..."]

@itemlist[

@item{Line 137 states that this is a @tech{local step} that @tech{each} of the participants performs.}

]

At this point, we can run the program and see its output by running @cmd{./reach run}.

@margin-note{If your version isn't working, look at the complete versions of the index.rsh and index.mjs files to make sure you copied everything down correctly!}

@margin-note{How come Alice and Bob's balance goes back to @litchar{100} each time?
It's because every time we run @exec{./reach run}, it starts a completely fresh instance of the testing network and creates new accounts for each player.}

@margin-note{How come the balances aren't exactly @litchar{100}, @litchar{105}, and @litchar{95}?
It's because Ethereum transactions cost "gas" to run.

If we had shown all the decimals, they'd look like this:

@verbatim{
Alice went from 10 to 14.999999999999687163.
Bob went from 10 to 4.999999999999978229.
}

Why does Alice win slightly less than Bob when she wins?
She has to pay to @tech{deploy} the contract, because she calls acc.deploy in her @tech{frontend}.

The @seclink["guide-deploymode"]{guide section on deployment} discusses how to avoid this difference.}

@margin-note{If your version isn't working, look at the complete versions of index.rsh and index.mjs to make sure you copied everything down correctly!}

Since the players act randomly, the results will be different every time.
Running this program, the outcome is as follows:

@verbatim{
Bob sees the final state: 
    ╔═══╦═══╦═══╗
    ║ O ║ X ║ X ║
    ╠═══╬═══╬═══╣
    ║   ║ X ║ O ║
    ╠═══╬═══╬═══╣
    ║ O ║ X ║ O ║
    ╚═══╩═══╩═══╝

Alice sees the final state: 
    ╔═══╦═══╦═══╗
    ║ O ║ X ║ X ║
    ╠═══╬═══╬═══╣
    ║   ║ X ║ O ║
    ╠═══╬═══╬═══╣
    ║ O ║ X ║ O ║
    ╚═══╩═══╩═══╝

Alice went from 100 to 104.9999
Bob went from 100 to 94.9999
Done!
}

Alice is pretty good at Tic-Tac-Toe!

@tech{Consensus networks} in general (and Reach specifically) guarantee that all participants agree on the outcome of their decentralized computation.
Indeed, this is where the name @tech{consensus network} comes from. They enable these distributed, and untrusted, parties to come to an agreement (or "consensus") about the intermediate states of a computation; and if they agree on the intermediate states, they will also agree on the output.
That's why, every time you run @exec{./reach run}, both Alice and Bob will see the same outcome.

In the @seclink["tic-tac-3"]{next section}, we'll add "pay-to-play" functionality to the tic-tac-toe engine.

@seclink["top"]{Main menu}

@section[#:tag "tic-tac-3"]{Pay-to-Play}
@(mint-scope "tic-tac-3")

In this section, we will add pay-to-play functionality to our game!

The first thing we have to do is add the fee matrix and a few fee handlers to the Reach script so it can access them for fee calculations.

@reachex[#:show-lines? #t "tic-tac-3/index.rsh"
         #:link #t
         'only 11 18 "      // ..."]

@itemlist[

@item{Line 12 defines the fee matrix, which contains the data that each side costs fee '2', each corner costs fee '3', and the center costs fee '4'.}

@item{Line 15 defines a function to multiply each item in the fee matrix by the corresponding item in the given board matrix.}

@item{Line 17 defines a small container function to take the cumulative sum of the matrix returned by the fee_realized function.}

]

Then, we pass the fee_mt variable to interact.getMove, so that the JavaScript code can print the fee for the given move.

@reachex[#:show-lines? #t "tic-tac-3/index.rsh"
         #:link #t
         'only 61 61 "      // ..."]

Once that is done, we have to modify what we tell the Reach program we'll be passing to the JavaScript getMove function.

@reachex[#:show-lines? #t "tic-tac-3/index.rsh"
         #:link #t
         'only 78 78 "      // ..."]

and add in the @litchar{Fees} type...

@reachex[#:show-lines? #t "tic-tac-3/index.rsh"
         #:link #t
         'only 3 3 "      // ..."]

Next, we'll actually modify what players pay.

@reachex[#:show-lines? #t "tic-tac-3/index.rsh"
         #:link #t
         'only 136 153 "      // ..."]

Now, the Reach file is fully set up for pay-to-play functionality. Since this is done, we'll move on to editing the JavaScript side of the app.

The only thing to change is the getMove function:

@reachex[#:mode js #:show-lines? #t "tic-tac-3/index.mjs"
         #:link #t
         'only 50 65 "      // ..."]

With that, you've incorporated pay-to-play functionality into your app!
The only visible difference in the output of the game is that the final balances will no longer be exactly 5 ETH (or ALGO, depending on which REACH_CONNECTOR_MODE you're running on) different from the starting balances.

Since the players act randomly, the results will be different every time.
Running this program, the outcome is as follows:

@verbatim{
Bob sees the final board: 
    ╔═══╦═══╦═══╗
    ║ X ║ O ║ O ║
    ╠═══╬═══╬═══╣
    ║ O ║ X ║ X ║
    ╠═══╬═══╬═══╣
    ║ X ║ O ║ X ║
    ╚═══╩═══╩═══╝

Alice sees the final board: 
    ╔═══╦═══╦═══╗
    ║ X ║ O ║ O ║
    ╠═══╬═══╬═══╣
    ║ O ║ X ║ X ║
    ╠═══╬═══╬═══╣
    ║ X ║ O ║ X ║
    ╚═══╩═══╩═══╝

Alice went from 1000 to 1002.8124.
Bob went from 1000 to 997.1874.
Done!
}

Our game is fully implemented, but there's a fatal flaw! Read the @seclink["tic-tac-5"]{next section} to find out why.

@seclink["top"]{Main menu}

@section[#:tag "tic-tac-5"]{Timeouts and Participation}
@(mint-scope "tic-tac-5")

In this section, we'll focus on a subtle issue that is important and unique to decentralized applications: @seclink["guide-timeout"]{non-participation}.

Non-participation refers to the act of one party ceasing to continue playing their role in an application.

In traditional client-server programs, like a web server, this would be the case of a client not sending any more requests to the server, or the server stopping sending responses to the client.
In these sorts of traditional programs, non-participation is an exceptional circumstance that normally leads to an error message for clients and, at most, a log entry for servers.
Sometimes traditional programs will need to recycle resources, like network ports, on non-participation, but they would have also needed to do that if the transaction ended by normal means.
In other words, for traditional client-server programs, it is not necessary for designers to meticulously consider the consequences of non-participation.

In contrast, decentralized applications must be designed with an eye for non-participation. What happens if Alice has pays her wager, but Bob never accepts? Alice's @tech{network tokens} would be locked inside of the @tech{contract} and lost to her.
Similarly, if Alice never submits her hand after Bob accepts and pays his wager, both their funds would be locked away forever.

@margin-note{Technically, in the first case, when Bob fails to start the application, Alice is not locked away from her funds. Since Bob's identity is not fixed until after his first message, she could start another instance of the game as the Bob role and win all of the funds (minus transaction costs of the @tech{consensus network}). In the second case, however, there would be no recourse for either party.}

In the rest of this section, we'll discuss how Reach helps address non-participation.
For a longer discussion, refer to @seclink["guide-timeout"]{the guide chapter on non-participation}.

In Reach, non-participation is handled through a "timeout" mechanism whereby each @tech{consensus transfer} can be paired with a @tech{step} that occurs for all @tech{participants} if the @tech{originator} of the @tech{consensus transfer} fails to make the required @tech{publication} before a particular @tech{time}.
We'll integrate this mechanism into our version of tic-tac-toe.

First, we'll modify the @tech{participant interact interface} to allow the @tech{frontend} to be informed that a timeout occurred.

@reachex[#:mode js #:show-lines? #t "tic-tac-4/index.rsh"
         #:link #t
         'only 80 80 "      // ..."]

@itemlist[

@item{Line 80 introduces a new method, informTimeout, that receives no arguments and returns no information.
We'll call this function when a timeout occurs.}

]

We'll make a slight tweak to our JavaScript @tech{frontend} to be able to receive this message and display it on the console.


@reachex[#:mode js #:show-lines? #t "tic-tac-4/index.mjs"
         #:link #t
         'only 69 72 "      // ..."]


Back in the Reach program, we'll define an identifier at the top of our program to use a standard deadline throughout the program.

@reachex[#:mode js #:show-lines? #t "tic-tac-4/index.rsh"
         #:link #t
         'only 88 88 "      // ..."]

@itemlist[

@item{Line 88 defines the deadline as 240 @tech{time delta} units, which are an abstraction of the underlying notion of @tech{time} in the @tech{consensus network}.
In many networks (including Ethereum) this refers to a number of blocks.}

]

Next, at the start of the Reach application, we'll define a helper function to inform each of the participants of the timeout by calling this new method.

@reachex[#:mode js #:show-lines? #t "tic-tac-4/index.rsh"
         #:link #t
         'only 95 99 "      // ..."]

@itemlist[

@item{Line 95 defines the function as an @tech{arrow expression}.}

@item{Line 96 has each of the participants perform a @tech{local step}.}

@item{Line 97 has them call the new informTimeout method.}

]

We won't change Alice's first message, because there is no consequence to her as a non-participant. If she doesn't start the game, then no one is any worse off.

However, we'll adjust Bob's first message. If he fails to participate, then Alice's initial wager will be lost to her.

@reachex[#:mode js #:show-lines? #t "tic-tac-4/index.rsh"
         #:link #t
         'only 112 112 "      // ..."]

@itemlist[

@item{Line 112 adds a timeout handler to Bob's @tech{publication}.}

]

The timeout handler specifies that if Bob does not complete this action within a @tech{time delta} of DEADLINE, then the application transitions to @tech{step} given by the arrow expression.
In this case, the timeout code is a call to closeTo, which is a Reach standard library function that has Alice send a message and transfer all of the funds in the @tech{contract} to herself, then call the given function afterwards.
This means that if Bob fails to publish his hand, then Alice will take her @tech{network tokens} back.

We will add a similar timeout handler to Alice's second message.

But in this case, Bob will be able to claim all of the funds if Alice doesn't participate.
You might think that it would be "fair" for Alice's funds to be returned to Alice and Bob's to Bob.
However, if we implemented it that way, then Alice would be wise to always timeout if she were going to lose, which she knows will happen, because she knows her hand and Bob's hand.

These are the only changes we need to make to the Reach program to make it robust against non-participation: seven lines!

If you were to run the program and see what happens, there would be no noticeable difference from the last chapter, but when we implement the next part, @seclink["tic-tac-6"]{Interaction and Independence}, you would be able to timeout the game.

@seclink["top"]{Main menu}

@section[#:tag "tic-tac-6"]{Interaction and Independence}
@(mint-scope "tic-tac-6")

In the last section, we made our tic-tac-toe run until there was a definitive winner.
In this section, we won't be making any changes to the Reach program itself.
Instead, we'll take a peek under the hood of @exec{reach run}, as well as build an interactive version of the game that can be played away from a private developer test network.

@(hrule)

In the past, when we've run @exec{./reach run}, it would create a Docker image just for our Reach program that contained a temporary Node.js package connecting our JavaScript @tech{frontend} to the Reach standard library and a fresh instance of a private developer test network.
Since we'll customize this, build a non-automated version of tic-tac-toe, and create the option of connecting to a real Ethereum network. we'll start by running @cmd{./reach scaffold}.

This will automatically generate the following files for us:

@itemlist[

@item{@exec{package.json} --- A Node.js package file that connects our @exec{index.mjs} to the Reach standard library.}

@item{@exec{Dockerfile} --- A Docker image script that builds our package efficiently and runs it.}

@item{@exec{docker-compose.yml} --- A Docker Compose script that connects our Docker image to a fresh instance of the Reach private developer test network.}

@item{@exec{Makefile} --- A @exec{Makefile} that easily rebuilds and runs the Docker image.}

]

We're going to leave the first two files unchanged.
You can look at them at @reachexlink["tic-tac-5/package.json"] and @reachexlink["tic-tac-5/Dockerfile"], but the details aren't especially important.
However, we'll customize the other two files.

First, let's look at the @reachexlink["tic-tac-5/docker-compose.yml"] file:

@reachex[#:mode yaml
         #:show-lines? #t "tic-tac-5/docker-compose.yml"
         #:link #t]

@itemlist[

@item{Lines 2 and 3 define a service for starting our application.
Your line 3 will say @litchar{tic-tac}, rather than @litchar{tic-tac-5}, if you've stayed in the same directory throughout the tutorial.}

@item{Lines 5 and 6 define the Reach private developer test network service for Ethereum.}

@item{Lines 7 through 24 define the Reach private developer test network service for Algorand.}

@item{Lines 25 through 73 define services that allow the application to be run with different networks; including line 24, which defines @litchar{reach-app-tic-tac-5-ETH-live} for connecting to a live network.}

@item{We'll also add lines 73 through 77 to define a @litchar{player} service that is our application with an open standard input, as well as two instances named @litchar{Alice} and @litchar{Bob}.}
]

With these in place, we can run @cmd{docker-compose run WHICH} where @exec{WHICH} is @litchar{reach-app-tic-tac-5-ETH-live} for either a live instance or @litchar{Alice} or @litchar{Bob} for a test instance.
If we use the live version, then we have to define the environment variable @envvar{ETH_NODE_URI} as the URI of our Ethereum node.

We'll modify the @reachexlink["tic-tac-5/Makefile"] to have commands to run each of these variants:

@reachex[#:mode makefile
         #:show-lines? #t "tic-tac-5/Makefile"
         #:link #t
         'only 29 39 ""]

However, if we try to run either of these, it will do the same thing it always has: create test accounts for each user and simulate a random game.
Let's modify the JavaScript @tech{frontend} and make them interactive.

@(hrule)

We'll start from scratch and show every line of the program again.
You'll see a lot of similarity between this and the last version but, for completeness, we'll show every line.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 1 6       "  // ..."]

@itemlist[

@item{Lines 1 and 2 are the same as before: importing the standard library and the backend.}

@item{Line 3 is new and imports a helpful library for simple console applications called @exec{ask.mjs} from the Reach standard library.
We'll see how these three functions are used below.}

]

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 32 35 "  // ..."]

@itemlist[

@item{Lines 32 through 35 ask the question whether they are playing as Alice and expect a "Yes" or "No" answer.
@jsin{ask} presents a prompt and collects a line of input until its argument does not error.
@jsin{yesno} errors if it is not given "y" or "n".}

]

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 48 60 "  // ..."]

@itemlist[

@item{Lines 48 through 60 present the user with the choice of creating a test account if they can or imputing a secret to load an existing account.}

@item{Line 53 creates the test account as before.}

@item{Line 59 loads the existing account.}

]

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 61 77 "  // ..."]

@itemlist[

@item{Lines 67 through 69 deploy the contract and print out public information (@jsin{ctc.getInfo}) that can be given to the other player.}

@item{Lines 72 through 76 request, parse, and process this information.}

]

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 79 82 "  // ..."]

Next, we define a few helper functions and start the participant interaction interface.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 86 89 "  // ..."]

First we define a timeout handler.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 112 122 "  // ..."]

Then, we define the @jsin{acceptWager} method.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 83 129 "  // ..."]

Next, we define the shared @jsin{getMove} method.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 130 132 "  // ..."]

Finally, the @jsin{endsWith} method.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-5/index.mjs"
         #:link #t
         'only 146 152 "  // ..."]

Lastly, we choose the appropriate backend function and await its completion.

We can now run @cmd{make build} to rebuild the images, then @cmd{make run-alice} in one terminal in this directory and @cmd{make run-bob} in another.

Here's an example run:

@verbatim{
$ make run-alice
Are you Alice?
y
Would you like to create an account? (only possible on devnet)
y
How much do you want to wager?
5
The contract is deployed as = 
{
"address":"0xae5307E387792F5f5a9327bddfE65aFF0608D2dA",
"creation_block":3,
"creator":"0x4e9F2Bc2c8b0DDA3b35727F27Be3c8ea76CBcf58",
"transactionHash":"0x32d9117ff2ddaa101be0a0742dd6499b62025f3068e341cb8656efdcfc9050b3"
}
Your balance is 999.9999
Alice publishes wager of 5 ETH

    ╔═══╦═══╦═══╗
    ║   ║   ║   ║
    ╠═══╬═══╬═══╣
    ║   ║   ║   ║
    ╠═══╬═══╬═══╣
    ║   ║   ║   ║
    ╚═══╩═══╩═══╝

Alice's turn.
Alice, what position will you play?
5
You played 5
Which maps to 4
Alice chooses move 4 from board above.
fee: 1.25 * 10**18
}

and

@verbatim{
$ make run-bob
Are you Alice?
n
Would you like to create an account? (Only possible on devnet.)
y
Please paste the contract information:
{
"address":"0xae5307E387792F5f5a9327bddfE65aFF0608D2dA",
"creation_block":3,
"creator":"0x4e9F2Bc2c8b0DDA3b35727F27Be3c8ea76CBcf58",
"transactionHash":"0x32d9117ff2ddaa101be0a0742dd6499b62025f3068e341cb8656efdcfc9050b3"
}
Your balance is 1000
Do you accept the wager of 5?
y

    ╔═══╦═══╦═══╗
    ║   ║   ║   ║
    ╠═══╬═══╬═══╣
    ║   ║ X ║   ║
    ╠═══╬═══╬═══╣
    ║   ║   ║   ║
    ╚═══╩═══╩═══╝

Bob's turn.
Bob, what position will you play?
}

Of course, when you run the exact amounts and addresses may be different.

@margin-note{If your version isn't working, look at the complete versions of the files mentioned in this chapter (see GitHub) to make sure you copied everything down correctly!}

If we were to edit @reachexlink["tic-tac-6/docker-compose.yml"], and move the @litchar{&default-app} on line 24 to line 51, we'd be able to test and run our application on Algorand instead of Ethereum.

Now our implementation of Tic-Tac-Toe is finished!
We are protected against timeouts and we can run interactively on non-test networks.

In this step, we made a command-line interface for our Reach program.
In @seclink["tic-tac-7"]{the next step}, we'll replace this with a web interface for the same Reach program.

@seclink["top"]{Main menu}

@section[#:tag "tic-tac-7"]{Web Interaction}
@(mint-scope "tic-tac-7")

In the last section, we made tic-tac-toe run as a command-line application, without any changes to the Reach program.
In this section, we again won't be making any changes to the Reach program.
Instead, we'll replace the command-line interface with a web interface.

We will use @link["https://reactjs.org/"]{React.js} for this tutorial, but the same principles apply to any web framework.

@margin-note{If you've never used React before, here are some basics about how it works:
@itemlist[
@item{React programs are JavaScript programs that use a special library that allows you to mix HTML inside of the body of your JavaScript.}
@item{React has a special compiler that combines a bundle of JavaScript programs, and all their dependencies, into one large file that can be deployed on a static web server.
This is called "packing".}
@item{When you're developing and testing with React, you run a special development web server that watches updates this packed file every time you modify your source files, so you don't have to constantly run the compiler.}
@item{Reach automates the process of starting this development server for you when you run @exec{./reach react} and gives you access to it at @tt{http://localhost:3000/}.}
]}

Similarly, in this tutorial, we assume that we will be deploying (and testing) with Ethereum.
Reach web applications rely on the web browser to provide access to a consensus network account and its associated wallet.
On Ethereum, the standard wallet is @link["https://metamask.io"]{MetaMask}.
If you want to test this code, you'll need to install it and set it up.
Furthermore, MetaMask does not support multiple active accounts, so if you want to test the game locally, you'll need to have two separate browser instances: one to act as Alice and another to act as Bob.

@(hrule)

The code in this section does not use the scaffolding from the previous section.
Reach comes with a convenience command for deleting scaffolded files:

@cmd{./reach unscaffold}

Similarly, you do not need the previous @tt{index.mjs} file, because we'll be writing it completely from scratch to use React.
You can run the following command to delete it:

@cmd{rm index.mjs}

Or, you can copy the @tt{index.rsh} file into a new directory and work from there.

@(hrule)

This code is supplemented with @reachexlink["tic-tac-7/index.css" "index.css"] and some @reachexlink["tic-tac-7/views" "views"].
These details are not specific to Reach, and are fairly trivial, so we will not explain the specifics of those files.
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

We will focus on @reachexlink["tic-tac-7/index.js"] because @reachexlink["tic-tac-7/index.rsh"] is the same as previous sections.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 1 9 "// ..."]

On lines 1 thru 6, we import our view code and CSS.
On line 7, we import the compiled @reachin{backend}.
On line 8, we import the @reachin{stdlib} as @reachin{reach}.

@(hrule)

To run on Algorand, change the import on line 8.

@reachin{import * as reach from '@"@"reach-sh/stdlib/ALGO'}

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 10 45 "// ..."]

On these lines we define a few helpful constants and defaults for later, some corresponding to the enumerations we defined in Reach.

@(hrule) @;; Explain App

We start defining @jsin{App} as a React component, and tell it what to do once it mounts, which is the React term for starting.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 55 127 "// ..."]

@itemlist[

 @item{On line 60, we initialize the component state to display @exviewref["tic-tac-7" "ConnectAccount"].}

 @item{On lines 66 thru 70, we hook into React's @jsin{componentDidMount} lifecycle event, which is called when the component starts.}

 @item{On line 73, we use @jsin{getDefaultAccount}, which accesses the default browser account.

  For example, when used with Ethereum, it can discover the currently-selected MetaMask account.}

 @item{On line 84, we use @jsin{getFaucet} to try and access the Reach developer testing network faucet.}

 @item{On line 86, if @jsin{getFaucet} was successful, we set the component state to display @exviewref["tic-tac-7" "FundAccount"].}

 @item{On line 91, if @jsin{getFaucet} was unsuccessful, we set the component state to skip to @exviewref["tic-tac-7" "DeployerOrAttacher"].}

 @item{On lines 115 - 127, we render the appropriate view from @reachexlink{tic-tac-7/views/AppViews.js}.}

]

 @;; Explain Player

Next, we will define @jsin{Player} as a React component, which will be extended by the specialized components for Alice and Bob.

@exviewfigs["tic-tac-7" "PlayerViews"
  '["GetHand" 8 32]]

Our Web frontend needs to implement the @tech{participant interact interface} for players, which we defined as:

@reachex[#:show-lines? #t "tic-tac-7/index.rsh"
         #:link #t
         'only 73 83 "// ..."]

We will provide these callbacks via the React component directly.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 136 241 "// ..."]

@itemlist[
 @item{On line 138, we provide the @jsin{random} callback}
 @item{On lines 142 thru 218, we provide the @jsin{getHand} callback.}
 @item{On lines 221 thru 235, we provide the @jsin{seeOutcome} and @jsin{informTimeout} callbacks,
  which set the component state to display @exviewref["tic-tac-7" "Done"], as well as a timeout page.}
 @item{On line 238, we define what happens when the user clicks a button:
  The @jsin{Promise} from line 150 is resolved.}
]

@exviewfigs["tic-tac-7" "PlayerViews"
  '["Done" 44 54]]

@(hrule) @;; explain Deployer

@;; TODO: rename Deployer->Alice, Attacher->Bob
Next, we will define @jsin{Deployer} as a React component for Alice, which extends @jsin{Player}.

@exviewfigs["tic-tac-7" "DeployerViews"
  '["SetWager" 20 38]
  '["Deploy" 40 53]]

Our web frontend needs to implement the @tech{participant interact interface} for Alice, which we defined as:

@reachex[#:show-lines? #t "tic-tac-7/index.rsh"
         #:link #t
         'only 78 80 "// ..."]

We will provide the @jsin{wager} value, and define some button handlers in order to trigger the deployment of the contract.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 244 283 "// ..."]

@itemlist[
 @item{On line 249, we set the component state to display @exviewref["tic-tac-7" "SetWager"].}
 @item{On line 250, we define what to do when the user clicks the @litchar{Set Wager} button which is to set the component state to display @exviewref["tic-tac-7" "Deploy"].}
 @item{On lines 260 thru 277, we define what to do when the user clicks the @litchar{Deploy} button.
 @item{On line 261, we call @jsin{acc.deploy}, which triggers a deploy of the contract.}
 @item{On line 263, we set the component state to display @exviewref["tic-tac-7" "Deploying"].}
 @item{On line 266, we set the @jsin{wager} property.}
 @item{On line 271, we start running the Reach program as Alice, using the @jsin{this} React component as the @tech{participant interact interface} object.}

 @item{On lines 274 and 275, we set the component state to display @exviewref["tic-tac-7" "WaitingForAttacher"] which displays the deployed contract info as JSON.}]}
 @item{On line 281, we render the appropriate view from @reachexlink{tic-tac-7/views/DeployerViews.js}.}
]

@exviewfigs["tic-tac-7" "DeployerViews"
  '["Deploying" 55 61]
  '["WaitingForAttacher" 63 90]]

@(hrule) @;; Explain Attacher

@exviewfigs["tic-tac-7" "AttacherViews"
  '["Attach" 18 39]
  '["Attaching" 41 49]]

Our web frontend needs to implement the @tech{participant interact interface} for Bob, which we defined as:

@reachex[#:show-lines? #t "tic-tac-7/index.rsh"
         #:link #t
         'only 81 83 "// ..."]

We will provide the @jsin{acceptWager} callback, and define some button handlers in order to attach to the deployed contract.

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 286 327 "// ..."]

@itemlist[
 @item{On line 291, we initialize the component state to display @exviewref["tic-tac-7" "Attach"].}
 @item{On lines 295 thru 321, we define what happens when the user clicks the @litchar{Attach} button.
 @item{On line 296, we call @jsin{acc.attach}}
 @item{On line 298, we set the component state to display @exviewref["tic-tac-7" "Attaching"].}
 @item{On line 300, we start running the Reach program as Bob, using the @jsin{this} React component as the @tech{participant interact interface} object.}]}
 @item{On lines 304 thru 314, we define the @jsin{acceptWager} callback.
 @item{On lines 308 thru 312, we set the component state to display @exviewref["tic-tac-7" "AcceptTerms"] and wait for a @jsin{Promise} which can be resolved via user interaction.}]}
 @item{On lines 316 thru 321, we define what happens when the user clicks the @litchar{Accept Terms and Pay Wager} button:
  the @jsin{Promise} from line 307 is resolved, and we set the component state to display @exviewref["tic-tac-7" "WaitingForTurn"].}
  
 @item{On line 325 we render the appropriate view from @reachexlink{tic-tac-7/views/AttacherViews.js}}
]

@exviewfigs["tic-tac-7" "AttacherViews"
  '["AcceptTerms" 51 70]
  '["WaitingForTurn" 72 81]]

@(hrule) @;; explain renderDOM

@reachex[#:mode js
         #:show-lines? #t "tic-tac-7/index.js"
         #:link #t
         'only 330 330 "// ..."]

Finally, we call a small helper function from @reachexlink{tic-tac-7/views/render.js} to render our app component.

@(hrule) @;; explain reach react and reach react-down

As a convenience for running the React development server, you can call:

@cmd{./reach react}

@(hrule)

To run the React development server with Algorand, take a look at the @seclink["algo"]{running on Algorand} section.

@(hrule) @;; explain npm install

If you'd like to instead use Reach in your own JavaScript project, you can call:

@cmd{npm install @"@"reach-sh/stdlib}

@margin-note{The Reach standard library is undergoing continual improvement and is updated often. If you are experiencing issues with the Node.js package, try updating!}

As usual, you can compile your Reach program @litchar{index.rsh} to the @jsin{backend} build artifact @litchar{build/index.main.mjs} with:

@cmd{./reach compile}

@(hrule) @;; conclusion

Now our implementation of tic-tac-toe is live in the browser!
We can leverage callbacks in the @tech{participant interact interface} to display to and gather information from the user, through the web UI framework of our choice.

If we wanted to deploy this application to the world, then we would take the static files that React produces and host them on a web server. These files embed your compiled Reach program, so there's nothing left to do.

@seclink["top"]{Main menu}

@section[#:tag "algo"]{Running on Algorand}

To run a Reach application on Algorand, a few changes are needed.

For a CLI dApp, the only thing needed to change from ETH to ALGO is to run the usual command with @exec{REACH_CONNECTOR_MODE=ALGO} before the @exec{./reach} keyword.

@margin-note{If you wanted to directly set the connection mode to ETH, just replace the @exec{ALGO} in the command with @exec{ETH}}

@(hrule)

In the web app, however, you must change the import of @exec{import * as reach from "reach-sh/stdlib/ETH";} to @exec{import * as reach from "reach-sh/stdlib/ALGO";}.

@(hrule)

For both CLI and web dApps, beware of the byte limit!
Algorand currently does not support compiled consensus calculations larger than 1000 bytes.
This can be particularly problematic when attempting to create more complex games with Reach.
For now, applications larger than the 1000 byte limit must be compiled and run with Ethereum.

However, there are ways to make more efficient calculations to lower the calculation bytes. Some examples include:

@itemlist[

@item{Using @exec{or} statements instead of many @exec{and} statements. For example, 

@verbatim{
    (
        ((b[0] && b[1] && b[2]) ? 1:0) 
        +((b[3] && b[4] && b[5]) ? 1:0)
        +((b[6] && b[7] && b[8]) ? 1:0)
        +((b[0] && b[3] && b[6]) ? 1:0)
        +((b[1] && b[4] && b[7]) ? 1:0)
        +((b[2] && b[5] && b[8]) ? 1:0)
        +((b[0] && b[4] && b[8]) ? 1:0)
        +((b[2] && b[4] && b[6]) ? 1:0)
    ) > 0
}

is an inefficient method of calculating the equivalent

@verbatim{
    (
        (b[0] && (((b[1]) && (b[2])) || ((b[3]) && (b[6]))))
        ||(b[8] && (((b[6]) && (b[7])) || ((b[2]) && (b[5]))))
        ||(b[4] && ((b[0] && b[8]) || (b[2] && b[6]) || (b[1] && b[7]) || (b[3] && b[5])))
    )
}
}

@item{Using @exec{if..else} statements instead of multiplication statements. For example, 

@verbatim{
    const fee_realized = (st, fee) => Array.iota(9).map((i) => fee[i] * (st[i] ? 1:0));
}

is an inefficient method of calculating the equivalent

@verbatim{
    const fee_realized = (st, fee) => Array.iota(9).map((i) => (st[i] ? fee[i]:0));
}
}

]

@seclink["top"]{Main menu}

@section[#:tag "trouble"]{Troubleshooting}

There are a few things that might go wrong with your Reach code that may not be because of something you did.

@itemlist[

@item{If, when running a React development server, you see an error like the following

@verbatim{
    ERROR: Cannot start service dev-server:
        driver failed programming external connectivity on endpoint tictac_dev-server_run_1
        (bf15551792f714f06cb88469d0a856b77c53a7a66f7df66f814b2745ca15b88b):
            Bind for 0.0.0.0:3000 failed: port is already allocated
}

then type @exec{sudo docker ps} into your command-line and find the container that says port 3000

@verbatim{
    CONTAINER ID   IMAGE                      COMMAND               PORTS
    c8c67d0eee4c   reachsh/react-runner:0.1   "npm run -- start"    0.0.0.0:3000->3000/tcp
}

and type @exec{sudo docker stop <container id>}.}


@item{When running any @exec{./reach} command, if an error like the following pops up:

@verbatim{
    docker: Got permission denied while trying to connect to the Docker daemon socket at unix:///var/run/docker.sock
}

then try using @exec{sudo} before the @exec{./reach} command.
} 

@item{When running a React server on ETH, if MetaMask and/or the browser console shows that there is an incorrect transaction nonce, please go into MetaMask > Settings > Advanced and reset your account.}

@item{If an error other than the two mentioned above pops up, please check that you have correctly copied the code from @hyperlink["https://www.github.com/squidKid-deluxe/reach--tic-tac-toe"]{GitHub}}.

]
