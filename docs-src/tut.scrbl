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

@reachex[#:show-lines? #t "tut-2a/tut.rsh"
         #:link "tut.rsh"]

@margin-note{Did you notice that @reachexlink["tut-2a.rsh" @exec{tut.rsh}] was a link in the box above the code sample?
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

@reachex[#:show-lines? #t "tut-2a/tut.mjs"
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

In the next step, we'll implement the logic of Rock, Paper, Scissors and our application will start doing something!

@section[#:tag "tut-2"]{Step 2: Rock, Paper, and Scissors}


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
