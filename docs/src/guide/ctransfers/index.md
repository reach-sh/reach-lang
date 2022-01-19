# {#guide-ctransfers} What do the different kinds of consensus transfers mean? `publish`, `pay`, `race`, `fork`, `parallelReduce`?

A fundamental concept in Reach is that the application starts in a detached birds-eye perspective called a step where each of the participants are acting completely independently.
These independent steps are punctuated by consensus transfers which cause all of the participants to come together and agree on the computation, which is called a consensus step.
:::note
These consensus steps are what happen "on-chain" and are executed by a so-called "smart contract".
:::

There are many ways to perform a consensus transfer in Reach and each is used for different reasons.
The variety can feel a bit bewildering for new users, but here is an intuitive guide to help you decide which you need for any given situation.

## Is a single participant responsible for the transfer?

If the entire DApp is waiting for a single participant to act, such as when at a play the entire theatre waits in anticipation for the stage hands to draw the curtains, then you either need a `{!rsh} pay` or `{!rsh} publish`.
If the single participant is sharing information, then you need a `{!rsh} publish`;
but if they are only paying a previously known amount, then you need a `{!rsh} pay`.
This kind of transfer always explicitly names the party acting, as in:
```reach
Auctioneer.publish(openingBid);
```

`{!rsh} pay` and `{!rsh} publish` without a `{!rsh} race` are for when one participant wants to do one thing.

## Are multiple participants able to cause the transfer, but each will provide different data?

If the entire DApp has a clear "next step", but multiple participants are able to provide the values used in that step, then you need a `{!rsh} race` to determine the participant used for a `{!rsh} publish` or `{!rsh} pay`.
For example, in a radio call-in contest, there is one chance for a listener to call-in first and win the prize.
If a participant value is participant class, then all of its actions are `{!rsh} race`s, because the Reach identifier does not represent a specific principal, but a member of a large class.

`{!rsh} race`, and participant classes are for when many participants want to do one thing.

## Are there multiple participants that each want to do different things?

If the DApp doesn't have a clear next step, but multiple participants each have a different option for what the next step should be, then you need a `{!rsh} fork`.
For example, a sale DApp might have a buy option and a bid option where the first immediately ends the computation, while the second moves it to an auction.

`{!rsh} fork` is for when many participants want to each do a different thing.

## Will these different things happen over and over until some condition is met?

If you are in a situation where a `{!rsh} race` or `{!rsh} fork` is needed, and the options are available repeatedly after small diversions, then you need a `{!rsh} parallelReduce`.
For example, in an auction, bidders repeatedly provide new bids as they compete to be the highest bidder before a time limit is reached.

## Summary

The main questions are:
+ How many participants can act? One or many?
+ How many things can be done? One or many?
+ How many times can this be done? Once or many?

If it happens many times, you want `{!rsh} parallelReduce`.
If many things could happen, you want `{!rsh} fork`.
If many participants can act, you want `{!rsh} race`.
Otherwise, you want `{!rsh} publish` or `{!rsh} pay`.

:::note
The above article uses the word "need" when talking about `{!rsh} fork` and `{!rsh} parallelReduce`, but actually you never need these, because they are just abbreviations of particular patterns of using the other features.
`{!rsh} fork` works by having each participant `{!rsh} race` to provide a `{!rsh} Data` instance, where the single consensus step does a case analysis on the `{!rsh} Data` to determine which code to run.
Similarly, `{!rsh} parallelReduce` is just an abbreviation of a `{!rsh} fork` within a `{!rsh} while` loop.
:::