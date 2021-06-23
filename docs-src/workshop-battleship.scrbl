#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-battleship")
@title[#:version reach-vers #:tag TAG]{Workshop: Battleship}

In this workshop, we'll design an application that allows users to wager against each other then play a simplified version of Battleship which each player selects their ship locations and guesses their opponents ship locations all in one turn.

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
 @item{This program involves 2 parties: The Deployer (Alice) who sets the wager and deploys the contract and the Attacher (Bob) who attaches to the contract and accepts the wager.}
 @item{The game starts by Alice placing a wager and deploying the contract. The contract info is returned to Alice to share with Bob.}
 @item{Bob uses the contract provided by Alice to attach to the contract.}
 @item{Bob can choose to accept the wager or timeout and end the game.}
 @item{Alice select where to place her ships.}
 @item{Bob selects where to place his ships.}
 @item{Alice selects where she thinks Bob placed his ships.}
 @item{Bob selects where he thinks Alice placed her ships.}
 @item{The number of correct guesses each has made on the location of thier opponent's ships is counted and either a winner is determined or a draw occurts}
]

