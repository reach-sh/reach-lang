#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-fomo-generalized")
@title[#:version reach-vers #:tag TAG]{Workshop: Fear of Missing Out Generalized}

In this workshop, we will extend our @seclink["workshop-fomo"]{Fear of Missing Out application}
with the ability to split the reward between the @tt{N} most recent Buyers.

In this version, the Funder will have the advantage that, if there are less than
@tt{N} Buyers, the Funder will earn the rewards for every absent Buyer. For example,
if the auction is set to have 5 winners, yet only 3 Buyers bid, the first three Buyers
will receive 1/5 of the funds each, and the Funder will receive the remaining 2/5 of the funds.

@(workshop-deps "workshop-fomo")
@(workshop-init TAG)

@(drstep-pr TAG)

Our problem analysis is practically the same as the original Fear of Missing Out application,
except for one difference:

@itemlist[
  @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's compare answers for how funds should change ownership in this generalized version:

@itemlist[
  @item{Buyers continually add funds to the balance during execution until the last @tt{N} Buyers, and potentially the Funder, split the balance.}
]

@(drstep-dd TAG)

The data type representation of this program will basically be the same as the
regular Fear of Missing Out program. However, instead of tracking the latest Buyer as an
@reachin{Address}, we will track the last @tt{N} Buyers as an @reachin{Array(Address, N)}.

You should take the time now to fill out the interaction interface for the participants.

@(drstep-dd-stop)

Our @tech{participant interact interface}, with the addition of some handy logging functions, looks like this so far:

@margin-note{It is worth noting that Reach does not support arbitrarily sized arrays, so we
could not determine @tt{NUM_OF_WINNERS} at runtime, e.g. from the interaction interface.
However, we can still write a program that is generic in the size of the array, then
specialize it when we compile.
}

@reachex[#:show-lines? #t "workshop-fomo-generalized/index.rsh"
         #:link #t
         'only 4 22 "  // ..."]

At this point, you can modify your JavaScript file (@tt{index.mjs}) to contain defintions of these values, although you may want to use a placeholders for the actual value.
When you're writing a Reach program, especially in the early phases, you should have these two files open side-by-side and update them in tandem as you're deciding the @tech{participant interact interface}.


@(drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication
and transfer among the participants. We should write down this structure as comments
in our program to serve as an outline and guide us in implementation. In our original Fear of Missing Out implementation, we outlined the pattern of communication as follows:

@reach{
  // 1. The Funder publishes the ticket price and deadline
  // 2. While the deadline has yet to be reached:
  //     2a. Allow a Buyer to purchase a ticket
  //     2b. Keep track of last Buyer
  // 3. Transfer the balance to the last person who bought a ticket
}

This outline will need to be updated for our generalized version. You should do this now, in your Reach program (@tt{index.rsh}).

@(drstep-cc-stop1)

Here's what we wrote for our outline:
@reach{
  // 1. The Funder publishes the ticket price and deadline
  // 2. While the deadline has yet to be reached:
  //     2a. Allow a Buyer to purchase a ticket
  //     2b. Keep track of the winners (last N Buyers)
  // 3. Divide the balance evenly amongst the winners.
  // 4. Transfer the reward to each winner.
}

Now, this outline needs to be converted to a real program.

@(drstep-cc-stop2)

The body of your application should look something like this:

@reach{
  // 1. The Funder publishes the ticket price and deadline
  Funder.publish(ticketPrice, deadline);

  const initialWinners = Array.replicate(NUM_OF_WINNERS, Funder);

  const [ keepGoing, winners, ticketsSold ] =
    // 2. While the deadline has yet to be reached:
    parallel_reduce([ true, initialWinners, 0 ])
      .invariant(balance() == ticketsSold * ticketPrice)
      .while(keepGoing)
      .case(
        Buyer,
        // 2a. Allow a Buyer to purchase a ticket
        (() => ({
          when: declassify(interact.shouldBuyTicket(ticketPrice)) })),
        (() => ticketPrice),
        () => {
          const buyer = this;
          // 2b. Keep track of the winners (last N Buyers)
          const idx = ticketsSold % NUM_OF_WINNERS;
          const newWinners =
            Array.set(winners, idx, buyer);
          return [ true, newWinners, ticketsSold + 1 ]; })
      .timeout(deadline, () => {
        race(Buyer, Funder).publish();
        return [ false, winners, ticketsSold ]; });

  // 3. Divide the balance evenly amongst the winners.
  transfer(balance() % NUM_OF_WINNERS).to(Funder);
  const reward = balance() / NUM_OF_WINNERS;

  // 4. Transfer the reward to each winner.
  winners.forEach(winner =>
    transfer(reward).to(winner));

  commit();
}

Extending this program to track an array of @reachin{Address}es, as opposed to a single
@reachin{Address} is fairly straightforward. We maintain an array of size @tt{NUM_OF_WINNERS}
and implement a ring buffer to keep it up to date with the most recent @tt{N} winners, as
demonstrated in step @tt{2b}.

Another aspect of this code worth highlighting is step @tt{3}.
We transfer @reachin{balance() % NUM_OF_WINNERS} to the winner because the total balance may not be evenly
divisible by the number of winners.

For example, if the ticket price is @tt{4 ETH}
and there are 10 tickets purchased by Buyers, then the total balance will be @tt{40 ETH}. However, if the
application is set to select 3 winners, then 40 cannot be evenly distributed to 3 participants. So, we
will transfer @tt{1 ETH} to the Funder, and split the remaining @tt{39 ETH} between the 3 Buyers.

@(drstep-ai TAG)

This program doesn't have many interesting properties to prove
as assertions, beyond the @tech{token linearity property}. The
only property of interest is the @reachin{parallel_reduce} invariant
which states that the balance must be equal to the number of tickets
sold multiplied by the ticket price.

@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @reachin{interact}.
In this case, our program is very simple and we expect you'll do a great job without further discussion.

@(drstep-ii-stop)

Let's look at our whole program now:

@reachex[#:show-lines? #t "workshop-fomo-generalized/index.rsh"
         #:link #t]

@(drstep-de TAG)

Next, it is time to test our program. As usual, we'll present a completely
automated test deployment, rather than an interactive one.

The program is fairly straightfoward to test. We just create test accounts for
the Funder and any number of Buyers. The decision to purchase a ticket by
a Buyer will rely simply on generating a random boolean.

@(drstep-de-stop)

Here's the JavaScript @tech{frontend} we wrote:

@reachex[#:show-lines? #t "workshop-fomo-generalized/index.mjs"
         #:link #t]

Let's see what it looks like when we run the program:

@verbatim{
$ ../reach run
Buyer #6 bought a ticket.
Buyer #3 bought a ticket.
...
Buyer #4 bought a ticket.
Buyer #1 bought a ticket.
Buyer #3 bought a ticket.
Buyer #8 bought a ticket.
Buyer #6 bought a ticket.
Funder saw they lost
Buyer #1 saw they lost
Buyer #5 saw they lost
Buyer #9 saw they lost
Buyer #0 saw they lost
Buyer #8 saw they won
Buyer #7 saw they lost
Buyer #2 saw they lost
Buyer #6 saw they won
Buyer #3 saw they won
Buyer #4 saw they lost
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

Great job!

You've now implemented a generalized Fear of Missing Out game. You can try extending
this application with additional features such as:

@itemlist[
  @item{Slightly increasing the ticket price with each purchase.}
  @item{Introducing a small payout system (dividends) to Buyers as the game progresses.
    e.g. every time the ring buffer is filled.}
]

If you found this workshop rewarding, please let us know on @(the-community-link)!

