#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-ctransfers"]{What do the different kinds of consensus transfers mean? @tt{publish}, @tt{pay}, @tt{race}, @tt{fork}, @tt{parallelReduce}?}

A fundamental concept in Reach is that the application starts in a detached birds-eye perspective called a @tech{step} where each of the @tech{participants} are acting completely independently.
These independent @tech{steps} are punctuated by @tech{consensus transfers} which cause all of the @tech{participants} to come together and agree on the computation, which is called a @tech{consensus step}.
@margin-note{These @tech{consensus steps} are what happen "on-chain" and are executed by a so-called "smart contract".}

There are many ways to perform a @tech{consensus transfer} in Reach and each is used for different reasons.
The variety can feel a bit bewildering for new users, but here is an intuitive guide to help you decide which you need for any given situation.

@section{Is a single participant responsible for the transfer?}

If the entire @|DApp| is waiting for a single @tech{participant} to act, such as when at a play the entire theatre waits in anticipation for the stage hands to draw the curtains, then you either need a @reachin{pay} or @reachin{publish}.
If the single participant is sharing information, then you need a @reachin{publish};
but if they are only paying a previously known amount, then you need a @reachin{pay}.
This kind of transfer always explicitly names the party acting, as in:
@reach{
 Auctioneer.publish(openingBid);
}

@reachin{pay} and @reachin{publish} without a @reachin{race} are for when one participant wants to do one thing.

@section{Are multiple participants able to cause the transfer, but each will provide different data?}

If the entire @|DApp| has a clear "next step", but multiple participants are able to provide the values used in that step, then you need a @reachin{race} to determine the participant used for a @reachin{publish} or @reachin{pay}.
For example, in a radio call-in contest, there is one chance for a listener to call-in first and win the prize.
If a participant value is @tech{participant class}, then all of its actions are @reachin{race}s, because the Reach identifier does not represent a specific principal, but a member of a large class.

@reachin{race}, and @tech{participant class}es are for when many participants want to do one thing.

@section{Are there multiple participants that each want to do different things?}

If the @|DApp| doesn't have a clear next step, but multiple participants each have a different option for what the next step should be, then you need a @reachin{fork}.
For example, a sale @|DApp| might have an buy option and a bid option where the first immediately ends the computation, while the second moves it to an auction.

@reachin{fork} is for when many participants want to each do a different thing.

@section{Will these different things happen over and over until some condition is met?}

If you are in a situation where a @reachin{race} or @reachin{fork} is needed, and the options are available repeatedly after small diversions, then you need a @reachin{parallelReduce}.
For example, in an auction, bidders repeatedly provide new bids as they compete to be the highest bidder before a time limit is reached.

@section{Summary}

The main questions are:
@itemlist[
@item{How many participants can act? One or many?}
@item{How many things can be done? One or many?}
@item{How many times can this be done? Once or many?}
]

If it happens many times, you want @reachin{parallelReduce}.
If many things could happen, you want @reachin{fork}.
If many participants can act, you want @reachin{race}.
Otherwise, you want @reachin{publish} or @reachin{pay}.

@margin-note{
The above article uses the word "need" when talking about @reachin{fork} and @reachin{parallelReduce}, but actually you never need these, because they are just abbreviations of particular patterns of using the other features.
@reachin{fork} works by having each participant @reachin{race} to provide a @reachin{Data} instance, where the single @tech{consensus step} does a case analysis on the @reachin{Data} to determine which code to run.
Similarly, @reachin{parallelReduce} is just an abbreviation of a @reachin{fork} within a @reachin{while} loop.
}
