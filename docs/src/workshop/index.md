# {#workshop} Workshop

The workshop is a collection of self-study projects to help you on your way to mastery building decentralized applications through practice by building specific projects in Reach.
It is designed to help direct you along the path of learning effective use of Reach and points out for you some of the design decisions that Reach programmers often make.

Unlike [the tutorial](##tut), it is not meant for you just to copy code and see what appears; instead, you're meant to work on your own to build a project yourself.
This way, you'll learn more and slowly be able to build your own project using Reach.

At the end of every project, we'll always show a "final" version with commentary about why we implemented it the way that we did.

:::note
We would love your feedback on any project, especially the parts that you struggled the most with.
This will help us improve the language, our developer tools, and the project materials.
The best way is through the [GitHub issue tracker](https://github.com/reach-sh/reach-lang/issues) or by messaging `@team` in [the Discord community](@{DISCORD}).
:::


We highly recommend going through [the overview](##overview) and [the tutorial](##tut) before continuing through the workshop.
However, you can do most workshops in any order.
If there's a pre-requisite, it will be explicitly mentioned.

Here are the current workshops, in the order we recommend doing them:
+ @{seclink("workshop-hash-lock")}
+ @{seclink("workshop-relay")}
+ @{seclink("workshop-trust-fund")}
+ @{seclink("workshop-fomo")}
+ @{seclink("workshop-fomo-generalized")}
+ @{seclink("workshop-rps-fair")}
+ @{seclink("workshop-rps-eff")}
+ @{seclink("workshop-rental")}
+ @{seclink("workshop-abstract-simul")}
+ @{seclink("workshop-guardian-account")}
+ @{seclink("workshop-utility")}
+ @{seclink("workshop-nim")}
+ @{seclink("workshop-ttt")}
+ @{seclink("workshop-secured-loan")}
+ @{seclink("workshop-atomic-swap")}
+ @{seclink("workshop-atomic-swap-auction")}
+ @{seclink("workshop-race")}
+ @{seclink("workshop-chicken-race")}
+ @{seclink("workshop-popularity-contest")}
+ @{seclink("workshop-nft-dumb")}
+ @{seclink("workshop-nft-auction")}
+ @{seclink("workshop-nft-tax")}
+ @{seclink("workshop-raffle")}
+ @{seclink("workshop-rent-seeking")}
+ @{seclink("workshop-remote")}
+ @{seclink("workshop-oracle")}
+ @{seclink("workshop-auction-te")}
+ @{seclink("workshop-crowdfund")}










## {#workshop-rps-fair} Fair Rock-Paper-Scissors

@{workshopDeps("tut")}

In this workshop, we observe that the [tutorial](##tut)'s version of _Rock, Paper, Scissors!_ is unfair in that Alice is responsible for more steps than Bob.
On most consensus networks, each action is charged a fee to run, so this means that Alice pays more than Bob for the game to be played.
How could you revise the application to make it fair, without requiring a change to the frontend?

@{workshopWIP()}

## {#workshop-rps-eff} Efficient Rock-Paper-Scissors

@{workshopDeps("workshop-rps-fair")}

In this workshop, we study how transaction costs on consensus networks can be understood as the constants hidden by asymptotic notations when determining the expense of an algorithm when run on a decentralized application.
In typical programming contexts, an algorithm that uses `3 log_2 n + 4 n` operations is considered equivalent to an algorithm that uses `5 log_4 n + 22 n` operations, because constants and bases are [ignored in asymptoptic analysis](https://en.wikipedia.org/wiki/Big_O_notation#Properties).
However, imagine that a program used `n` local computations and `m` consensus computations.
We'll call `n` the "computations" and `m` the "communications".
In this case, the computations are free from the perspective of the consensus network, because they don't cost network tokens, while the communications cost their price in gas, plus the fee to run them.
Therefore, it is often economically efficient to increase `n` so that `m` can be smaller.

For example, in the context of the [tutorial](##tut)'s version of _Rock, Paper, Scissors!_, the application uses `2 + 3r` communications for a game with `r` rounds.
This is because it takes two communications to set up the loop, then each round of the loop takes three communications.
We could make a more complicated version of the application that is optimized in two ways.

First, we could optimize for the common case of when there is no draw and bundle a hand into the opening messages, and use `3 + 3(r - 1) = 3r` communications for `r` rounds, for a saving of two communications.
This would slightly increase the complexity of our program by duplicating the submission of hands, but we could easily abstract this into a Reach function.

Second, we could bundle `k` hands into each communication, so that the number of communications is `3(r//k)` for `r` rounds for a reduction of communications by `k` times.
This is possible through Reach's ability to deal with array values.
The exact value of `k` would be chosen empirically based on the relative difference in cost between increased message sizes and computations versus the fixed cost of having any transaction at all on the consensus network.
Reach's ability to abstract away the details of communication patterns allows us to write this program abstractly and only specify the value of `k` as a compile-time parameter.

This is a general strategy that is regularly employed in efficient decentralized applications: although a textbook algorithm might say to use a setup phase and many round trips as you divide a space in half each time, it might be vastly more efficient on an actual network to apply meaning-preserving transformations like merging the setup into the loop and dividing the space by a much larger constant, like one hundred.

@{workshopWIP()}

## {#workshop-rental} Rental Agreement

@{workshopDeps("tut")}

In this workshop, we consider a scenario where Alice and Bob are engaged in an indefinite rental agreement where they both made a security deposit to be given to whomever leaves the arrangement first.
At some regular interval, both participants submit whether they would like to `Stay` or `Leave`.
If they both decide to `Stay`, then they wait for the next round.
If they both decide to `Leave`, then they both get back their deposit.
But if only one wants to `Leave`, then both deposits go to the participant that wants to `Stay`, based on the assumption that they are harmed by being surprised at the change in circumstances in the next interval.

This scenario demonstrates the value of decentralization, because in traditional institutions, one party is typically empowered as always dictating the decision to the other who must take it, as is the case for most landlords; or, the two parties must play "chicken" with each other to see who will flinch first, as is often the case in divorces; or, the two parties must pay a third-party to act as the arbitrator.

It turns out that this scenario is structurally identical to _Rock, Paper, Scissors!_, except with different "hands" and a different pay-out structure.

@{workshopWIP("rental")}

## {#workshop-abstract-simul} Simultaneous Games

@{workshopDeps("workshop-rental")}

In this workshop, we generalize from [the tutorial](##tut) and [the rental workshop](##workshop-rental) to build an abstract library that models any game with simultaneous play.
This shows the power of Reach's ability to construct and reason about communication abstractions.

@{workshopWIP("abstract-simul")}

## {#workshop-guardian-account} Guardian Account

@{workshopDeps("workshop-trust-fund")}

In @{seclink("workshop-trust-fund")}, we developed an application where the entire contents of a trust fund devolve to the receipient after a certain time.
In this workshop, we revisit this problem and instead allow portions of the funds to be removed by the funder at her discretion.
This is like a "multi-signature wallet", where one account originates the funds and approves transactions, while another account solely spends those funds.
It might be used to give a child an "allowance" from their trust fund.
It could be modified to allow the funder to add funds over time if the fund runs low.
This is the first workshop on the "transfer funds" track that introduces `{!rsh} while` statements.

@{workshopWIP("multisig")}

## {#workshop-utility} Periodic Payment

@{workshopDeps("workshop-guardian-account")}

In @{seclink("workshop-guardian-account")}, we developed an application where a funder has discretion over the disbursement on portions of a pre-funded account.
In this workshop, we modify this application so that the funder has no discretion and automatically approves transfers out, but the receiver is restricted to only asking for a certain amount at a time and at a particular interval.
This could be used to make a periodic payment to a utility company, for example.

@{workshopWIP()}

## {#workshop-nim} Nim

@{workshopDeps("tut")}

In [the tutorial](##tut), we built a version of _Rock, Paper, Scissors!_ where two parties can wager over the results of the game.
This application has an interesting communication pattern with information hiding through cryptographic commitments, but the application logic is simple and uninteresting.
In this workshop, we implement a version of [Nim](https://en.wikipedia.org/wiki/Nim), where the communication structure is simpler, since it is a [combinatorial game](https://en.wikipedia.org/wiki/Combinatorial_game_theory), but has a more interesting application logic.
Thus, this workshop demonstrates using more interesting data-structures in Reach programs, as well as using compile-time abstractions to simplify DApp structure.

@{workshopWIP("nim")}

## {#workshop-ttt} Tic-Tac-Toe

@{workshopDeps("workshop-nim")}

Like @{seclink("workshop-nim")}, this workshop develops an implementation of the [combinatorial game](https://en.wikipedia.org/wiki/Combinatorial_game_theory), [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe).
This workshop demonstrates the use of arrays and more advanced Reach data-structures.
We'll discuss two variations of this same application:
one where the consensus network verifies the moves of each player;
and, another where the consensus network allows illegal moves to be disputed by the other player.
These variations demonstrate two DApp patterns that occur commonly in existing DApp designs, each with different usage constraints and performance trade-offs.

@{workshopWIP("ttt")}

## {#workshop-secured-loan} Secured Loan

@{workshopDeps("workshop-trust-fund")}

In this workshop, we implement a [secured loan](https://en.wikipedia.org/wiki/Secured_loan), wherein a Borrower posts collateral and terms to the public, and an arbitrary Lender gives a loan, then after some maturity, the Borrower either repays the loan, plus interest, or sacrifices the collateral.
It is easy to model this scenario using only network tokens, but it would be useless, because the premise of a loan is that the participants believe the Borrower can make better use of the funds than the Lender.
However, many consensus networks also support custom fungible assets that are like tokens.
If this is the case, then a secured loan will typically have different assets as the loan amount and the collateral amount.
Thus, the interest rate for the loan is essentially the Lender's prediction of the movement of the exchange rate between the two currencies over the lifetime of the loan. In this workshop, the Borrower pledges non-network tokens as collateral for the loan.


@{workshopWIP("secured-loan")}

## {#workshop-atomic-swap} Atomic Swap

@{workshopDeps("tut")}

In this workshop, we implement an atomic swap, or sell order, wherein a Seller offers an amount of an asset, X of A, in exchange for a prescribed amount of another asset, Y of B, and waits for a Buyer to complete the other side of the trade.
This workshop demonstrates the use of non-network tokens.

@{workshopWIP("atomic-swap")}

## {#workshop-atomic-swap-auction} Atomic Swap Auction

@{workshopDeps("workshop-atomic-swap")}

In this workshop, we extend the [Atomic Swap](##workshop-atomic-swap) workshop by allowing the Seller to solicit bids for their X of A in a prescribed asset B and accept whichever buyer is willing to provide the most before a preset time.

@{workshopWIP("atomic-swap-auction")}

## {#workshop-race} Race

@{workshopDeps("tut")}

In this workshop, we implement the example of the `{!rsh} race` expression discussed in [the guide section on races](##guide-race).
This provides an introduction to races, as well as a cautionary tale on their danger.

@{workshopWIP("race")}

## {#workshop-chicken-race} Chicken

@{workshopDeps("workshop-race")}

In this workshop, we implement a game of [Chicken](https://en.wikipedia.org/wiki/Chicken_(game)), where each player submits an equal wager, and then competes with the other to submit more transactions to the consensus before a deadline.
In other words, they must decide between the risk of losing the entire pot and the risk of overspending on transaction costs.
Like @{seclink("workshop-race")}, this demonstrates the deadweight losses associated with `{!rsh} race`s, as discussed in [the guide section on races](##guide-race).

@{workshopWIP("chicken-race")}

## {#workshop-popularity-contest} Popularity Contest

@{workshopDeps("workshop-chicken-race")}

In this workshop, we implement a [two-party winner-takes-all vote](https://en.wikipedia.org/wiki/Plurality_voting), where a pollster proposes two candidates---Alice and Bob---along with a voting price and a deadline, then a participant class of voters each pay and cast their ballot.
Once the deadline passes, the winning candidate takes the entire pot.
This workshop introduces effective use of participant classes and `{!rsh} parallelReduce`.

@{workshopWIP("popularity-contest")}

## {#workshop-nft-dumb} Simple NFT

@{workshopDeps("workshop-race")}

In this workshop, we implement a trivial [non-fungible token](https://en.wikipedia.org/wiki/Non-fungible_token) (NFT) where a single creator creates a unique item and initial owns it.
The creator and all subsequent owners may transfer ownership of the unique item to a different owner and so on.
This workshop uses a participant class to represent owners and is a kind of trivial template before we explore more interesting and in-depth variants of the NFT concept.

@{workshopWIP("nft-dumb")}

## {#workshop-nft-auction} NFT with Auction

@{workshopDeps("workshop-nft-dumb")}

In this workshop, we extend @{seclink("workshop-nft-dumb")} by adding an auction when the current owner is ready to sell their NFT.
This could be extended to give creators (or royalty rights holders) a percentage of the auction yield.

@{workshopWIP("nft-auction")}

## {#workshop-nft-tax} NFT with Royalties and Harberger Tax

@{workshopDeps("workshop-nft-dumb")}

In this workshop, we extend @{seclink("workshop-nft-dumb")} by incorporating a Harberger Tax (c.f. [Arnold Harberger](https://en.wikipedia.org/wiki/Arnold_Harberger)) where owners must state a price at which they are willing to part with the asset and pay a percentage of that price to the creator.
They can update this price upwards by paying an additional tax, or decrease it without cost (to free themselves of the asset).
This represents an interesting place in the NFT design space where utility increasing transfers are immediate and creators receive royalties.

## {#workshop-raffle} Raffle

@{workshopDeps("workshop-popularity-contest")}

In this workshop, we implement a [raffle](https://en.wikipedia.org/wiki/Raffle), where a sponsor starts a timed raffle and a participant class of ticket buyers each buy tickets. This workshop contains two interesting ideas: first, it uses linear state through the `{!rsh} Map` structure; second, it uses a commitment pattern structure to acquire safe randomness from the set of buyers.

@{workshopWIP("raffle")}

## {#workshop-rent-seeking} Rent Seeking

@{workshopDeps("workshop-raffle")}

In this workshop, we implement a [rent-seeking](https://en.wikipedia.org/wiki/Rent-seeking) competition, where a sponsor attempts to sell a prize (e.g. 10 ETH) and a number of bidders seek to acquire the prize by bidding for it (e.g. 5 ETH). Unlike a "normal" auction, however, the bidders lose the money they bid to the sponsor. Thus, while the winner may get more than the bid, and the sponsor may get more than they give away, there is a net transfer away from the bidders to the sponsor. This program also uses linear state.

@{workshopWIP("rent-seeking")}

## {#workshop-remote} Remote Objects

@{workshopDeps("tut")}

In this workshop, we demonstrate interaction with a remote object implemented externally to Reach.
Rather than connect to an existing contract, this example includes testing code to launch a bespoke contract simply to demonstrate how remote interactions can occur.
It may be representative of a DApp that is implemented simultaneously in Reach and the low-level language of the chosen consensus network, in this case Solidity.

@{workshopWIP("remote")}

## {#workshop-oracle} Oracle

@{workshopDeps("tut")}

In this workshop, we implement a centralized [oracle](https://en.wikipedia.org/wiki/Oracle_machine) for some property not otherwise computable within a Reach program.
Typically oracles are used to connect physical data from outside a consensus network to the agents of the network, such as by reading physical sensors, like a thermometer, and posting the information inside the network.

@{workshopWIP()}

## {#workshop-auction-te} Timed English Auction

@{workshopDeps("tut")}

In this workshop, we implement a timed [English auction](https://en.wikipedia.org/wiki/English_auction) wherein a Seller auctions off the right for a Bidder to decide the argument to call a certain external contract method with.
This demonstrates the use of collective operations in Reach with finite state.

@{workshopWIP()}

## {#workshop-crowdfund} Crowd-funding

@{workshopDeps("workshop-auction-te")}

In this workshop, we implement a [crowdfunding campaign](https://en.wikipedia.org/wiki/Crowdfunding) wherein a Fundraiser requests a funding amount, which is provided by Donors if a reserve is reached before the funding window closes, after which it is either disbursed to the Fundraiser, or returned to the Donors.

@{workshopWIP()}

