#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "tut" #:style 'toc]{Tutorial}

This tutorial walks through the creation of a simple decentralized application.
It contains everything you need to know to build and test this application.
If you want a broad overview before diving in it, we recommend reading @seclink["overview"]{the overview} first.
On the hand, if this is too simple, then you may want to look at @seclink["howtos"]{some how-to guides} for larger projects or @seclink["ref"]{the reference manual} for the minute details of Reach.

If you're ready, click through to the first step!

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "tut-1"]{Step 1: Install and Initialize}

Reach is designed to work on POSIX systems with @link["https://www.docker.com/get-started"]{Docker} and @link["https://docs.docker.com/compose/install/"]{Docker Compose}. installed.
You'll know that you have them install if you can run

@commandline{docker --version}

and

@commandline{docker-compose --version}

Once you've confirmed that they are installed, choose a directory for this project. We recommend

@commandline{mkdir -p ~/reach/tut && cd ~/reach/tut}

Next, install Reach by downloading it from @hyperlink["https://github.com/reach-sh/reach-lang"]{GitHub} by running

@commandline{curl https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach -o reach ; chmod +x reach}

You'll know that the download worked if you can run

@commandline{./reach version}

Since Reach is Dockerized, when you first use it, you'll need to download the images it uses.
This will happen automatically when you first use it, but you can do it manually now by running

@commandline{./reach update}

You'll know that everything is in order if you can run

@commandline{./reach compile --help}

@(hrule)

Now that your Reach installation is in order, you should open a text editor and get ready to write your first Reach application!

@section[#:tag "tut-2"]{Step 2: Rock, Paper, and Scissors}

XXX

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
