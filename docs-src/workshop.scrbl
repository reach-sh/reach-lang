#lang scribble/manual
@(require "lib.rkt" racket/match)

@title[#:version reach-vers #:tag "workshop" #:style 'toc]{Workshop}

The workshop is a collection of self-study projects to help you on your way to mastery building decentralized applications through practice by building specific projects in Reach.
It is designed to help direct you along the path of learning effective use of Reach and points out for you some of the design decisions that Reach programmers often make.

Unlike @seclink["tut"]{the tutorial}, it is not meant for you just to copy code and see what appears; instead, you're meant to work on your own to build a project yourself.
This way, you'll learn more and slowly be able to build your own project using Reach.

At the end of every project, we'll always show a "final" version with commentary about why we implemented it the way that we did.

@margin-note{We would love your feedback on any project, especially the parts that you struggled the most with.
This will help us improve the language, our developer tools, and the project materials.
The best way is through the @link["https://github.com/reach-sh/reach-lang/issues"]{GitHub issue tracker} or by messaging @litchar{@"@"team} in @(the-community-link).}

We highly recommend going through @seclink["overview"]{the overview} and @seclink["tut"]{the tutorial} before continuing through the workshop.
However, you can do most workshops in any order.
If there's a pre-requisite, it will be explicitly mentioned.

Here are the current workshops, in the order we recommend doing them:

@local-table-of-contents[#:style 'immediate-only]

@include-section["workshop-hash-lock.scrbl"]
@include-section["workshop-relay.scrbl"]
@include-section["workshop-trust-fund.scrbl"]
@include-section["workshop-fomo.scrbl"]
@include-section["workshop-fomo-generalized.scrbl"]
@include-section["workshop-battleship.scrbl"]

@(define (WIP/XXX . args)
  @margin-note{This page is a placeholder for a future more detailed workshop.
  You could try to implement yourself though, given the sketch above!
  @(match args
    [(list) ""]
    [(list x) @list{If you'd like to see a draft version of our code, please visit @link[(format "https://github.com/reach-sh/reach-lang/tree/master/examples/~a" x)]{@tt{examples/@|x|}} in our GitHub repository}])})

@section[#:tag "workshop-rps-fair"]{Workshop: Fair Rock-Paper-Scissors}

@(workshop-deps "tut")

In this workshop, we observe that the @seclink["tut"]{tutorial}'s version of @RPS is unfair in that Alice is responsible for more steps than Bob.
On most @tech{consensus networks}, each action is charged a fee to run, so this means that Alice pays more than Bob for the game to be played.
How could you revise the application to make it fair, without requiring a change to the @tech{frontend}?

@(WIP/XXX)

@section[#:tag "workshop-rps-eff"]{Workshop: Efficient Rock-Paper-Scissors}

@(workshop-deps "workshop-rps-fair")

In this workshop, we study how transaction costs on @tech{consensus networks} can be understood as the constants hidden by asymptotic notations when determining the expense of an algorithm when run on a decentralized application.
In typical programming contexts, an algorithm that uses @tt{3 log_2 n + 4 n} operations is consider equivalent to an algorithm that uses @tt{5 log_4 n + 22 n} operations, because constants and bases are @link["https://en.wikipedia.org/wiki/Big_O_notation#Properties"]{ignored in asymptoptic analysis}.
However, imagine that a program used @tt{n} local computations and @tt{m} consensus computations.
We'll call @tt{n} the "computations" and @tt{m} the "communications".
In this case, the computations are free from the perspective of the @tech{consensus network}, because they don't cost @tech{network tokens}, while the communication cost their price in gas, plus the fee to run them.
Therefore, it is often economically efficient to increase @tt{n} so that @tt{m} can be smaller.

For example, in the context of @seclink["tut"]{tutorial}'s version of @|RPS|, the application uses @tt{2 + 3r} communications for a game with @tt{r} rounds.
This is because it takes two communications to set up the loop, then each round of the loop takes three communications.
We could make a more complicated version of the application that is optimized in two ways.

First, we could optimize for the common case of when there is no draw and bundle a hand into the opening messages, and use @tt{3 + 3(r - 1) = 3r} communications for @tt{r} rounds, for a saving of two communications.
This would slightly increase the complexity of our program by duplicating the submission of hands, but we could easily abstract this into a Reach function.

Second, we could bundle @tt{k} hands into each communication, so that the number of communications is @tt{3(r//k)} for @tt{r} rounds for a reduction of communications by @tt{k} times.
This is possible through Reach's ability to deal with array values.
The exact value of @tt{k} would be chosen empirically based on the relative difference in cost between increase message sizes and computations versus the fixed cost of having any transaction at all on the @tech{consensus network}.
Reach's ability to abstract away the details of communication patterns also us to write this program abstractly and only specify the value of @tt{k} as a compile-time parameter.

This is a general strategy that is regularly employed in efficient decentralized applications: although a textbook algorithm might say to use a setup phase and many round trips as you divide a space in half each time, it might be vastly more efficient on an actual network to apply meaning-preserving transformations like merging the setup into the loop and dividing the space by much larger constant, like one hundred.

@(WIP/XXX)

@section[#:tag "workshop-rental"]{Workshop: Rental Agreement}

@(workshop-deps "tut")

In this workshop, we consider a scenario where Alice and Bob are engaged in indefinite rental agreement where they both made a security deposit to be given to whomever leaves the arrangement first.
At some regular interval, both participants submit whether they would like to @litchar{Stay} or @litchar{Leave}.
If they both decide to @litchar{Stay}, then they wait for the next round.
If they both decide to @litchar{Leave}, then they both get back their deposit.
But if only one wants to @litchar{Leave}, then both deposits go to the participant that wants to @litchar{Stay}, based on the assumption that they are harmed by being surprised at the change in circumstances in the next interval.

This scenario demonstrates the value of decentralization, because in traditional institutions, one party is typically empowered as always dictating the decision to other and must take it, as is the case for most landlords; or, the two parties must play "chicken" with each other to see who will flinch first, as is often the case in divorces; or, the two parties must pay a third-party to act as the arbitrator.

It turns out that this scenario is structurally identical to @|RPS|, except with different "hands" and a different pay-out structure.

@(WIP/XXX "rental")

@section[#:tag "workshop-abstract-simul"]{Workshop: Simultaneous Games}

@(workshop-deps "workshop-rental")

In this workshop, we generalize from @seclink["tut"]{the tutorial} and @seclink["workshop-rental"]{the rental workshop} to build an abstract library that models any game with simultaneous play.
This shows the power of Reach's ability to construct and reason about communication abstractions.

@(WIP/XXX "abstract-simul")

@section[#:tag "workshop-guardian-account"]{Workshop: Guardian Account}

@(workshop-deps "workshop-trust-fund")

In @secref["workshop-trust-fund"], we developed an application where the entire contents of trust fund devolve to the receipient after a certain time.
In this workshop, we revisit this problem and instead allow portions of the funds to be removed by the funder at her discretion.
This is like a "multi-signature wallet", where one account originates the funds and approves transactions, while another account solely spends those funds.
It might be used to give a child an "allowance" from their trust fund.
It could be modified to allow the funder to add funds over time if the fund runs low.
This is the first workshop on the "transfer funds" track that introduces @reachin{while} statements.

@(WIP/XXX "multisig")

@section[#:tag "workshop-utility"]{Workshop: Periodic Payment}

@(workshop-deps "workshop-guardian-account")

In @secref["workshop-guardian-account"], we developed an application where a funder has discretion over the disbursement on portions of a pre-funded account.
In this workshop, we modify this application so that the funder has no discretion and automatically approves transfers out, but the receiver is restricted to only asking for a certain amount at a time and at a particular interval.
This could be used to make a periodic payment to a utility company, for example.

@(WIP/XXX)

@section[#:tag "workshop-nim"]{Workshop: Nim}

@(workshop-deps "tut")

In @seclink["tut"]{the tutorial}, we built a version of @|RPS| where two parties can wager over the results of the game.
This application has an interesting communication pattern with information hiding through cryptographic commitments, but the application logic is simple and uninteresting.
In this workshop, we implement a version of @link["https://en.wikipedia.org/wiki/Nim"]{Nim}, where the communication structure is simpler, since it is a @link["https://en.wikipedia.org/wiki/Combinatorial_game_theory"]{combinatorial game}, but has a more interesting application logic.
Thus, this workshop demonstrates using more interesting data-structures in Reach programs, as well as using compile-time abstractions to simplify DApp structure.

@(WIP/XXX "nim")

@section[#:tag "workshop-ttt"]{Workshop: Tic-Tac-Toe}

@(workshop-deps "workshop-nim")

Like @secref["workshop-nim"], this workshop develops an implementation of the @link["https://en.wikipedia.org/wiki/Combinatorial_game_theory"]{combinatorial game}, @link["https://en.wikipedia.org/wiki/Tic-tac-toe"]{Tic-tac-toe}.
This workshop demonstrates the use of arrays and more advanced Reach data-structures.
We'll discuss two variations of this same application:
one where the consensus network verifies the moves of each player;
and, another where the consensus network allows illegal moves to be disputed by the other player.
These variations demonstrate two DApp patterns that occur commonly in existing DApp designs, each with different usage constraints and performance trade-offs.

@(WIP/XXX "ttt")

@section[#:tag "workshop-secured-loan"]{Workshop: Secured Loan}

@(workshop-deps "workshop-trust-fund")

In this workshop, we implement a @link["https://en.wikipedia.org/wiki/Secured_loan"]{secured loan}, wherein a Borrower posts collateral and terms to the public, and an arbitrary Lender gives a loan, then after some maturity, the Borrower either repays the loan, plus interest, or sacrifices the collateral.
It is easy to model this scenario using only @tech{network tokens}, but it would be useless, because the premise of a loan is that the participants believe the Borrower can make better use of the funds than the Lender.
However, many @tech{consensus network} also support custom fungible assets that are like tokens.
If this is the case, then a secured loan will typically have different assets as the loan amount and the collateral amount.
Thus, the interest rate for the loan is essentially the Lender's prediction of the movement of the exchange rate between the two currencies over the lifetime of the loan. In this workshop, the Borrower pledges @tech{non-network tokens} as collateral for the loan.


@(WIP/XXX "secured-loan")

@section[#:tag "workshop-atomic-swap"]{Workshop: Atomic Swap}

@(workshop-deps "tut")

In this workshop, we implement an atomic swap, or sell order, wherein a Seller offers an amount of an asset, X of A, in exchange for a prescribed amount of another asset, Y of B, and waits for a Buyer to complete the other side of the trade.
This workshop demonstrates the use of @tech{non-network tokens}.

@(WIP/XXX "atomic-swap")

@section[#:tag "workshop-atomic-swap-auction"]{Workshop: Atomic Swap Auction}

@(workshop-deps "workshop-atomic-swap")

In this workshop, we extend the @seclink["workshop-atomic-swap"]{Atomic Swap} workshop by allowing the Seller to solicit bids for their X of A in a prescribed asset B and accept whichever buyer is willing to provide the most before a preset time.

@(WIP/XXX "atomic-swap-auction")

@section[#:tag "workshop-race"]{Workshop: Race}

@(workshop-deps "tut")

In this workshop, we implement the example of the @reachin{race} expression discussed in @seclink["guide-race"]{the guide section on races}.
This provides an introduction to races, as well as a cautionary tale on their danger.

@(WIP/XXX "race")

@section[#:tag "workshop-chicken-race"]{Workshop: Chicken}

@(workshop-deps "workshop-race")

In this workshop, we implement a game of @link["https://en.wikipedia.org/wiki/Chicken_(game)"]{Chicken}, where each player submits an equal wager, and then competes with the other to submit more transactions to the consensus before a deadline.
In other words, they must decide between the risk of losing the entire pot and the risk of overspending on transaction costs.
Like @secref["workshop-race"], this demonstrates the deadweight losses associated with @reachin{race}s, as discussed in @seclink["guide-race"]{the guide section on races}.

@(WIP/XXX "chicken-race")

@section[#:tag "workshop-popularity-contest"]{Workshop: Popularity Contest}

@(workshop-deps "workshop-chicken-race")

In this workshop, we implement a @link["https://en.wikipedia.org/wiki/Plurality_voting"]{two-party winner-takes-all vote}, where a pollster proposes two candidates---Alice and Bob---along with a voting price and a deadline, then a @tech{participant class} of voters each pay and cast their ballot.
Once the deadline passes, the winning candidates takes the entire pot.
This workshop introduces effective use of @tech{participant class}es and @reachin{parallelReduce}.

@(WIP/XXX "popularity-contest")

@section[#:tag "workshop-nft-dumb"]{Workshop: Simple NFT}

@(workshop-deps "workshop-race")

In this workshop, we implement a trivial @link["https://en.wikipedia.org/wiki/Non-fungible_token"]{non-fungible token} (NFT) where a single creator creates a unique item and initial owns it.
The creator and all subsequent owners may transfer ownership of the unique item to a different owner and so on.
This workshop uses a @tech{participant class} to represent owners and is a kind of trial temple before we explore more interesting and in-depth variants of the NFT concept.

@(WIP/XXX "nft-dumb")

@section[#:tag "workshop-nft-auction"]{Workshop: NFT with Auction}

@(workshop-deps "workshop-nft-dumb")

In this workshop, we extend @secref["workshop-nft-dumb"] by adding an auction when the current owner is ready to sell their NFT.
This could be extended to give creator (or royalty rights holders) a percentage of the auction yield.

@(WIP/XXX "nft-auction")

@section[#:tag "workshop-nft-tax"]{Workshop: NFT with Royalties and Harberger Tax}

@(workshop-deps "workshop-nft-dumb")

In this workshop, we extend @secref["workshop-nft-dumb"] workshop by incorporating a Harberger Tax (c.f. @link["https://en.wikipedia.org/wiki/Arnold_Harberger"]{Arnold Harberger}) where owners must state a price at which they are willing to part with the asset and pay a percentage of that price to the creator.
They can update this price upwards by paying an additional tax, or decrease it without cost (to free themselves of the asset.)
This represents an interesting place in the NFT design space where utility increasing transfers are immediate and creators receive royalties.

@section[#:tag "workshop-raffle"]{Workshop: Raffle}

@(workshop-deps "workshop-popularity-contest")

In this workshop, we implement a @link["https://en.wikipedia.org/wiki/Raffle"]{raffle}, where a sponsor starts a timed raffle and a @tech{participant class} of ticket buyers each buy tickets. This workshop contains two interesting ideas: first, it uses @tech{linear state} through the @reachin{Map} structure; second, it uses an commitment pattern structure to acquire safe randomness from the set of buyers.

@(WIP/XXX "raffle")

@section[#:tag "workshop-rent-seeking"]{Workshop: Rent Seeking}

@(workshop-deps "workshop-raffle")

In this workshop, we implement a @link["https://en.wikipedia.org/wiki/Rent-seeking"]{rent-seeking} competition, where a sponsor attempts to sell a prize (e.g. 10 ETH) and a number of bidders seek to acquire the prize by bidding for it (e.g. 5 ETH). Unlike a "normal" auction, however, the bidders lose the money they bid to the sponsor. Thus, while the winner may get more than the bid, and the sponsor may get more than they give away, there is a net transfer away from the bidders to the sponsor. This program also uses @tech{linear state}.

@(WIP/XXX "rent-seeking")

@section[#:tag "workshop-remote"]{Workshop: Remote Objects}

@(workshop-deps "tut")

In this workshop, we demonstrate interaction with a @tech{remote object} implemented externally to Reach.
Rather than connect to an existing @tech{contract}, this example includes testing code to launch a bespoke @tech{contract} simply to demonstrate how remote interactions can occur.
It may be representative of a @|DApp| that is implemented simultaneously in Reach and the low-level language of the chosen @tech{consensus network}, in this case Solidity.

@(WIP/XXX "remote")

@section[#:tag "workshop-oracle"]{Workshop: Oracle}

@(workshop-deps "tut")

In this workshop, we implement a centralized @link["https://en.wikipedia.org/wiki/Oracle_machine"]{oracle} for some property not otherwise computable within a Reach program.
Typically oracles are used to connect physical data from outside a @tech{consensus network} to the agents of the network, such as by reading physical sensors, like a thermometer, and posting the information inside the network.

@(WIP/XXX)

@section[#:tag "workshop-auction-te"]{Workshop: Timed English Auction}

@(workshop-deps "tut")

In this workshop, we implement a timed @link["https://en.wikipedia.org/wiki/English_auction"]{English auction} wherein a Seller auctions off the right for a Bidder to decide the argument to call a certain external @tech{contract} method with.
This demonstrates the use of collective operations in Reach with finite state.

@(WIP/XXX)

@section[#:tag "workshop-crowdfund"]{Workshop: Crowd-funding}

@(workshop-deps "workshop-auction-te")

In this workshop, we implement a @link["https://en.wikipedia.org/wiki/Crowdfunding"]{crowdfunding campaign} wherein Fundraiser requests a funding amount, which is provided by Donors if a reserve is reached before the funding window closes, after which it is either disbursed to the Fundraiser, or returned to the Donors.

@(WIP/XXX)

