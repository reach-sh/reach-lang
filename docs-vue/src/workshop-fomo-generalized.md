



# {#TAG} Workshop: Fear of Missing Out Generalized


In this workshop, we will extend our [Fear of Missing Out application](##workshop-fomo)
with the ability to split the reward between the `N` most recent Buyers.

In this version, the Funder will have the advantage that, if there are less than
`N` Buyers, the Funder will earn the rewards for every absent Buyer. For example,
if the auction is set to have 5 winners, yet only 3 Buyers bid, the first three Buyers
will receive 1/5 of the funds each, and the Funder will receive the remaining 2/5 of the funds.

XXX (workshop-deps "workshop-fomo")
XXX (workshop-init TAG)

XXX (drstep-pr TAG)

Our problem analysis is practically the same as the original Fear of Missing Out application,
except for one difference:

+ What funds change ownership during the application and how?


XXX (drstep-pr-stop)

Let's compare answers for how funds should change ownership in this generalized version:

+ Buyers continually add funds to the balance during execution until the last `N` Buyers, and potentially the Funder, split the balance.


XXX (drstep-dd TAG)

The data type representation of this program will basically be the same as the
regular Fear of Missing Out program. However, instead of tracking the latest Buyer as an
`Address`, we will track the last `N` Buyers as an `Array(Address, N)`.
XXX (drstep-dd-datatype-mn)

You should take the time now to fill out the interaction interface for the participants.

XXX (drstep-dd-stop)

Our participant interact interface, with the addition of some handy logging functions, looks like this so far:

::: note
It is worth noting that Reach does not support arbitrarily sized arrays, so we
could not determine `NUM_OF_WINNERS` at runtime, e.g. from the interaction interface.
However, we can still write a program that is generic in the size of the array, then
specialize it when we compile.
:::

@[code{5-23}](@reach-lang/examples/workshop-fomo-generalized/index.rsh)

At this point, you can modify your JavaScript file (`index.mjs`) to contain defintions of these values, although you may want to use a placeholders for the actual value.
When you're writing a Reach program, especially in the early phases, you should have these two files open side-by-side and update them in tandem as you're deciding the participant interact interface.


XXX (drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication
and transfer among the participants. We should write down this structure as comments
in our program to serve as an outline and guide us in implementation. In our original Fear of Missing Out implementation, we outlined the pattern of communication as follows:

```reach
// 1. The Funder publishes the ticket price and deadline
// 2. While the deadline has yet to be reached:
//     2a. Allow a Buyer to purchase a ticket
//     2b. Keep track of last Buyer
// 3. Transfer the balance to the last person who bought a ticket
```


This outline will need to be updated for our generalized version. You should do this now, in your Reach program (`index.rsh`).

XXX (drstep-cc-stop1)

Here's what we wrote for our outline:
```reach
// 1. The Funder publishes the ticket price and deadline
// 2. While the deadline has yet to be reached:
//     2a. Allow a Buyer to purchase a ticket
//     2b. Keep track of the winners (last N Buyers)
// 3. Divide the balance evenly amongst the winners.
// 4. Transfer the reward to each winner.
```


Now, this outline needs to be converted to a real program.

XXX (drstep-cc-stop2)

The body of your application should look something like this:

```reach
// 1. The Funder publishes the ticket price and deadline
Funder.publish(ticketPrice, deadline);

const initialWinners = Array.replicate(NUM_OF_WINNERS, Funder);

const [ keepGoing, winners, ticketsSold ] =
  // 2. While the deadline has yet to be reached:
  parallelReduce([ true, initialWinners, 0 ])
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
```


Extending this program to track an array of `Address`es, as opposed to a single
`Address` is fairly straightforward. We maintain an array of size `NUM_OF_WINNERS`
and implement a ring buffer to keep it up to date with the most recent `N` winners, as
demonstrated in step `2b`.

Another aspect of this code worth highlighting is step `3`.
We transfer `balance() % NUM_OF_WINNERS` to the winner because the total balance may not be evenly
divisible by the number of winners.

For example, if the ticket price is `4 ETH`
and there are 10 tickets purchased by Buyers, then the total balance will be `40 ETH`. However, if the
application is set to select 3 winners, then 40 cannot be evenly distributed to 3 participants. So, we
will transfer `1 ETH` to the Funder, and split the remaining `39 ETH` between the 3 Buyers.

XXX (drstep-ai TAG)

This program doesn't have many interesting properties to prove
as assertions, beyond the token linearity property. The
only property of interest is the `parallelReduce` invariant
which states that the balance must be equal to the number of tickets
sold multiplied by the ticket price.

XXX (drstep-ii TAG)

Next, we need to insert the appropriate calls to `interact`.
In this case, our program is very simple and we expect you'll do a great job without further discussion.

XXX (drstep-ii-stop)

Let's look at our whole program now:

@[code](@reach-lang/examples/workshop-fomo-generalized/index.rsh)

XXX (drstep-de TAG)

Next, it is time to test our program. As usual, we'll present a completely
automated test deployment, rather than an interactive one.

The program is fairly straightfoward to test. We just create test accounts for
the Funder and any number of Buyers. The decision to purchase a ticket by
a Buyer will rely simply on generating a random boolean.

XXX (drstep-de-stop)

Here's the JavaScript frontend we wrote:

@[code](@reach-lang/examples/workshop-fomo-generalized/index.mjs)

Let's see what it looks like when we run the program:

```
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
```


## {#(format ~a-dns TAG)} Discussion and Next Steps

Great job!

You've now implemented a generalized Fear of Missing Out game. You can try extending
this application with additional features such as:

+ Slightly increasing the ticket price with each purchase.
+ Introducing a small payout system (dividends) to Buyers as the game progresses.
e.g. every time the ring buffer is filled.


If you found this workshop rewarding, please let us know on <CommunityLink />!

