#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-fomo")
@title[#:version reach-vers #:tag TAG]{Workshop: Fear of Missing Out}
@author[(author+email "Chris Nevers" "cnevers@reach.sh")]

In this workshop, we'll design an application that allows a Funder
to create an auction where participants may purchase tickets.
The Funder sets a ticket price and a relative deadline. When
a Buyer purchases a ticket, the deadline is reset. Whomever is the
last person to buy a ticket—when the deadline finally hits—wins
the entire balance. This program is based off of the crypto game,
@link["https://fomo3d.hostedwiki.co/"]{FOMO3DGame}.

This workshop utilizes @tech{participant class}es to represent Buyers, which allows us to handle multiple participants in a generic way.

@(workshop-deps)
@(workshop-init TAG)

@(drstep-pr TAG)

First, we should think over the details of the application and
answer some questions to help reason about the implementation of
the program.

You should write the answer to the following questions in your
Reach program (@tt{index.rsh}) using a comment.
@reachin{/* Remember comments are written like this. */}

@itemlist[
  @item{Who is involved in this application?}
  @item{What information do they know at the start of the program?}
  @item{What information are they going to discover and use
  in the program?}
  @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's see how your answers compare to ours:

@itemlist[
  @item{This program involves two parties: a Funder who deploys the
    auction and the Buyers who purchase tickets.}
  @item{The Funder knows the deadline and the ticket price at the
  start of the application.}
  @item{The Buyers do not know anything of the deadline or ticket price when the application begins.}
  @item{The Funder does not learn anything during the program execution.}
  @item{The Buyers learn of the ticket price and deadline during the
  program execution.}
  @item{Buyers continually add funds to the balance during execution until one Buyer wins the entire balance.}
]

It's okay if some of your answers differ from ours!

@(drstep-dd TAG)

After problem analysis, we need to decide how we will represent the information
in the program:
@itemlist[
  @item{What data type will represent the deadline set by the Funder?}
  @item{What data type will represent the ticket price set by the Funder?}
  @item{What data type will represent the Buyer's decision to purchase a ticket?}
]

Now that we've decided what data types to use, we need to determine how the programs will obtain this information. We need to outline the @tech{participant interact interface} for each participant.

@itemlist[
  @item{What @tech{participant interact interface} will Funder use?}
  @item{What @tech{participant interact interface} will Buyer use?}
]

Revisit the problem analysis section when completing this section. Whenever
a participant starts off with some knowledge, that will be a field in the
@reachin{interact} object.
If they learn something, then it will be an argument to a function.
If they provide something later, then it will be the result of a function.

You should write your answers in your Reach file (@tt{index.rsh}) as the @tech{participant interact interface} for each of the participants.

@(drstep-dd-stop)

Let's compare your answers with ours:

@itemlist[
  @item{The deadline will be represented with an @reachin{UInt}, as it is
  a relative time delta signifying a change in block numbers.}
  @item{The @tt{ticketPrice} will be represented with an @reachin{UInt}}
  @item{The decision to buy a ticket will be represented by a function @reachin{Fun([UInt], Bool)}}
]

Our @tech{participant interact interface}, with the addition of some handy logging functions, looks like this so far:

@reachex[#:show-lines? #t "workshop-fomo/index.rsh"
         #:link #t
         'only 3 21 "  // ..."]


At this point, you can modify your JavaScript file (@tt{index.mjs}) to contain defintions of these values, although you may want to use a placeholders for the actual value.
When you're writing a Reach program, especially in the early phases, you should have these two files open side-by-side and update them in tandem as you're deciding the @tech{participant interact interface}.

@(drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication
and transfer among the participants. We should write down this structure as comments
in our program to serve as an outline and guide us in implementation.
For example, for the @seclink["tut"]{tutorial} version of @emph{Rock, Paper, Scissors!}, we might write:
@reach{
 // Alice publishes the wager and pays it
 // Bob accepts the wager and pays it
 // While there's a draw
 //  Alice publishes her hand secretly
 //  Bob publishes his hand publicly
 //  Alice reveals her hand
 //  The consensus ensures it's the same hand as before
 // The consensus pays out the wager
}

You should do this now, in your Reach program (@tt{index.rsh}).

@(drstep-cc-stop1)

Here's what we wrote for our outline:
@reach{
  // 1. The Funder publishes the ticket price and deadline
  // 2. While the deadline has yet to be reached:
  //     2a. Allow a Buyer to purchase a ticket
  //     2b. Keep track of last Buyer
  // 3. Transfer the balance to the last person who bought a ticket
}

Now, this outline needs to be converted to a real program.

@(drstep-cc-stop2)

The body of your application should look something like this:

@reach{
  // 1. The Funder publishes the ticket price and deadline
  Funder.publish(ticketPrice, deadline);

  const [ keepGoing, winner, ticketsSold ] =
    // 2. While the deadline has yet to be reached
    parallelReduce([ true, Funder, 0 ])
      .invariant(balance() == ticketsSold * ticketPrice)
      .while(keepGoing)
      .case(
        Buyer,
        // 2a. Allow a Buyer to purchase a ticket
        (() => ({
          when: declassify(interact.shouldBuyTicket(ticketPrice)),
        })),
        (() => ticketPrice),
        (() => {
          const buyer = this;
          Buyer.only(() => interact.showPurchase(buyer));
          // 2b. Keep track of last Buyer
          return [ true, buyer, ticketsSold + 1 ];
        })
      )
      .timeout(deadline, () => {
        race(Buyer, Funder).publish();
        return [ false, winner, ticketsSold ]});

  // 3. Transfer the balance to the last person who bought a ticket
  transfer(balance()).to(winner);
  commit();
}

We use @reachin{parallelReduce} to allow Buyers to purchase tickets until
the deadline passes and accumulate the current winner. We maintain the invariant
that the balance must be equal to the number of tickets sold multiplied by the
ticket price.

@(drstep-ai TAG)

This program doesn't have many interesting properties to prove
as assertions, beyond the @tech{token linearity property}. The
only property of interest is the @reachin{parallelReduce} invariant
which states that the balance must be equal to the number of tickets
sold multiplied by the ticket price.


@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @reachin{interact}.
In this case, our program is very simple and we expect you'll do a great job without further discussion.

@(drstep-ii-stop)

Let's look at our whole program now:

@reachex[#:show-lines? #t "workshop-fomo/index.rsh"
         #:link #t]

@(drstep-de TAG)

Next, it is time to test our program. As usual, we'll present a completely
automated test deployment, rather than an interactive one.

The program is fairly straightfoward to test. We just create test accounts for
the Funder and any number of Buyers. The decision to purchase a ticket by
a Buyer will rely simply on generating a random boolean.

@(drstep-de-stop)

Here's the JavaScript @tech{frontend} we wrote:

@reachex[#:show-lines? #t "workshop-fomo/index.mjs"
         #:link #t]

Let's see what it looks like when we run the program:

@verbatim{
$ ../reach run
Buyer 0 bought a ticket.
Buyer 3 bought a ticket.
...
Buyer 5 bought a ticket.
Buyer 9 bought a ticket.
Buyer 1 bought a ticket.
Funder saw 0x94CAC1b24C1f7b0EBAD4A51797dE5d59A39910C4 won.
Buyer 5 saw they lost.
Buyer 1 saw they won.
Buyer 6 saw they lost.
Buyer 0 saw they lost.
Buyer 4 saw they lost.
Buyer 8 saw they lost.
Buyer 2 saw they lost.
Buyer 7 saw they lost.
Buyer 3 saw they lost.
Buyer 9 saw they lost.
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

Great job!

If you found this workshop rewarding, please let us know on @(the-community-link)!

If you'd like to make this application a little more interesting, maybe you'd like
to extend this program to make the last @tt{N} buyers split the winnings. Check out
@seclink["workshop-fomo-generalized"]{Fear of Missing Out Generalized} for
our solution!

