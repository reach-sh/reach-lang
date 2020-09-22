#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-trust-fund")
@title[#:version reach-vers #:tag TAG]{Workshop: Trust Fund}

In this workshop, we'll look at yet another strategy for transferring funds, but in this version, we'll think about it as establishing a "trust fund": a funder will establish an account for the receiver, which they must wait a certain amount of time to access, and if they do not, then it reverts to the funder, and if the funder does not claim it, then it is dormant and any third party can remove the funds.
You could think of this as a variant of the @seclink["workshop-relay"]{relay account}, with a mandatory waiting period and two fallbacks on @seclink["guide-timeout"]{non-participation}.

@(workshop-deps "workshop-relay")
@(workshop-init TAG)

@(drstep-pr TAG)

For this workshop, we'll provide some constraints on your solution and problem analysis, since we'd like you to explore writing a Reach program with a specific design.

The overall purpose of this application is so that:
@itemlist[
 @item{The Funder must decide an amount of funds to provide, as well as all of the other parameters of the application.}
 @item{The Funder will know the identity of the Receiver at the beginning.}
 @item{Whomever ever ultimately receives the funds transfers it to themselves.}
]

With this in mind, let's answer the questions:
@itemlist[
 @item{What are the participants of the application?}
 @item{What information do they know at the start of the program?}
 @item{What information are they going to discover and use in the program?}
 @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's see how your answers compare to our answers:

@itemlist[
 @item{This applicaiton involves three participants: the Funder, the Receiver, and a Bystander.}
 @item{The Funder knows the identity of the Receiver, the amount, as well as the maturity of the fund, and the delays beforewhich it will be declared dormant or forsook.}
 @item{The Receiver and the Bystander don't learn or provide anything, aside from the existence of the fund and its maturity.}
 @item{The funds start with the Funder and then move to either the Receiver, the Funder, or the Bystander, depending on when they are claimed.}
]

@(drstep-dd TAG)

The next step of designing our program is representing this information in our program and deciding the @tech{participant interact interface} for each participant.
In this application, we'll be using a new concept of Reach: the @tech{time delta}.
The trust fund has a "maturity", as well as the lengths of time before which the fund is forsook or abandoned.
In the fiat world, these would likely be expressed as real time durations, like months and years.
However, on most @tech{consensus networks} there is an abstraction of time into something like a "block height", which represents the number of rounds of consensus which have reached their conclusion.
There is a loose relationship of these notions to real-time, but most networks do not guarantee any particular connection.
(Indeed, such a connection between the abstract world of consensus networks and the "real" world is typically provided by an @seclink["workshop-oracle"]{oracle}.)
Reach abstracts the details of particular consensus networks away into the concept of a @tech{time delta}, which is represented by an integer in Reach programs, and used in positions that reference time.

With that knowledge in hand,

@(drstep-dd-stop)

Let's compare notes again.
Here's what we wrote in our program:

@reachex[#:show-lines? #t "workshop-trust-fund/index.rsh"
         #:link #t
         'only 1 21 "      // ..."]

We've represented most values as @reachin{UInt256} fields, and created a "common" interface that has a series of signals for the different phases of the application: one for when the account is @reachin{funded}, one for when the particular participant is @reachin{ready} to extract the funds, and finally one for when they have successfuly @reachin{recvd} (received) them.

@(drstep-cc TAG)

Now, we can write down the structure of communication and action in our application.
Try this on your own based on your experience with @secref["workshop-trust-fund"].

@(drstep-cc-stop1)

Here's what we wrote:
@reach{
 // 1. The Funder publishes the parameters of the fund and makes the initial deposit.
 // 2. The consensus remembers who the Receiver is.
 // 3. Everyone waits for the fund to mature.
 // 4. The Receiver may extract the funds with a deadline of `refund`.
 // 5. The Funder may extract the funds with a deadline of `dormant`.
 // 6. The Bystander may extract the funds with no deadline.
}

The next step is to convert this pattern into actual program code using @reachin{publish}, @reachin{pay}, and @reachin{commit}.
However, this program gives us the opportunity to look at a few more features of Reach.

First, how do we implement step three, where each party waits for the fund to mature?
Reach has a primitive named @reachin{wait} which causes this to happen.
This may only occur in a @tech{step}, which is the same context where @reachin{publish} may occur.
This primitive, however, doesn't just cause the @emph{participants} to wait, instead it guarantees that the entire computation waits.
In other words, this means that the @tech{contract} will ensure that the later steps do not occur until after the waiting time.

Second, how do we implement steps four and five, where there is a deadline for an action to take place?
Reach @tech{publication} steps take an option called @reachin{.timeout} that specifies an alternative computation to occur if the first does not take place before the deadline.
The syntax looks like: @reachin{publish().timeout(deadline, () => alternative)}, which uses the @tech{arrow expression} syntax for specifying the alternative computation.

Finally, we hope you notice that steps four, five, and six are extremely similar.
Consider trying to write a function that is used three times to implement all of them!

@(drstep-cc-stop2)

The body of your application should look something like:
@reach{
 // 1. The Funder publishes the parameters of the fund and makes the initial deposit.
 Funder.publish(receiverAddr, payment, maturity, refund, dormant )
   .pay(payment);

 // 2. The consensus remembers who the Receiver is.
 Receiver.set(receiverAddr);
 commit();

 // 3. Everyone waits for the fund to mature.
 wait(maturity);

 // 4. The Receiver may extract the funds with a deadline of `refund`.
 Receiver.publish()
   .timeout(refund,
     () => {
      // 5. The Funder may extract the funds with a deadline of `dormant`.
       Funder.publish()
         .timeout(dormant,
           () => {
             // 6. The Bystander may extract the funds with no deadline.
             Bystander.publish();
             transfer(payment).to(Bystander);
             commit();
             exit(); });
        transfer(payment).to(Funder);
        commit();
        exit(); });
 transfer(payment).to(Receiver);
 commit();
 exit();
}

@margin-note{If you'd like to see how you might contain the repetition into a function, keep reading!}

@(drstep-ai TAG)

As usual, we should consider what assertions we can add to our program, but this program doesn't have any interesting properties to prove, so we'll move on.
Or rather, all of its interesting properties are the ones automatically included in all Reach programs, like that the funds are used linearly and nothing is left over in the account at the end, or that the protocol steps must be received before the corresponding deadlines.

@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @reachin{interact}.
In this case, our program is very simple and we expect you'll do a great job without further discussion.
However, if you want to simplify things, you might like to use @reachin{each} to signal to all the parties that the account is funded, rather than duplicating the interaction code over and over.

@(drstep-ii-stop)

Let's look at our whole program now:

@reachex[#:show-lines? #t "workshop-trust-fund/index.rsh"
         #:link #t]

@itemlist[
 @item{Lines 33 and 34 use @reachin{each} to run the same code block @reachin{only} in each of the given participants.}
 @item{Lines 51 through 59 abstract the duplicate copied repeated structure of the program into three calls to the same function.}
 @item{Lines 37 through 49 define this function as one that abstracts over who is permitted to extract the funds and whether there is a deadline.}
]

This program demonstrates some of the remarkable features of Reach: we were able to abstract away a pattern of communication into a function and use it repeatedly and in different ways.
Behind the scenes, when Reach compiles this program into a @tech{contract}, it will derive a four step protocol with implicit state to check that the appropriate participant takes their action only when allowed.

@(drstep-de TAG)

Next, it is time to test our program.
As usual, we'll present a completely automated test deployment, rather than an interactive one.
This means that we'll have to have our participants purposefully "miss" their deadlines so we can see that the timeouts and deadlines work correctly.
We'll implement it by abstracting away the test into a function of two parameters: booleans that decide whether the Receiver and Funder (respectively) should miss their deadline.
We'll implement this miss by using the standard library function @jsin{stdlib.wait} which takes a @tech{time delta} encoded as a number.
This function is like @reachin{wait}, except it is local only to a single participant and has no bearing on the rules of the application.
It's just a convenience mechanism for allowing time to pass on the consensus network.

We highly recommend that you try to implement a test setup like this yourself; when you're done, scroll down to see our solution.

@(drstep-de-stop)

Here's the JavaScript @tech{frontend} we wrote:

@reachex[#:show-lines? #t "workshop-trust-fund/index.mjs"
         #:link #t]

The most interesting part of this program is on lines 20 through 23 when we optionally cause a delay in the participant after they receive the signal that the account is funded.

Let's see what it looks like when we run this program:

@verbatim{
$ ../reach run
Begin demo with funder delay(0) and receiver delay(0).
Receiver sees that the account is funded.
Bystander sees that the account is funded.
Funder sees that the account is funded.
Receiver is ready to receive the funds.
Receiver received the funds.
Funder has a balance of 89.99926093
Receiver has a balance of 109.999956574
Bystander has a balance of 100.0

Begin demo with funder delay(0) and receiver delay(21).
Receiver sees that the account is funded.
Receiver begins to wait...
Bystander sees that the account is funded.
Funder sees that the account is funded.
Funder is ready to receive the funds.
Receiver is ready to receive the funds.
Funder received the funds.
Funder has a balance of 99.999217452
Receiver has a balance of 99.99995488
Bystander has a balance of 100.0

Begin demo with funder delay(31) and receiver delay(21).
Receiver sees that the account is funded.
Receiver begins to wait...
Bystander sees that the account is funded.
Funder sees that the account is funded.
Funder begins to wait...
Receiver is ready to receive the funds.
Bystander is ready to receive the funds.
Funder is ready to receive the funds.
Bystander received the funds.
Funder has a balance of 89.99921581
Receiver has a balance of 100.0
Bystander has a balance of 109.999956956
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

Great job!
You could use this application today and start put your child's college funds away for safe keeping!
Although, perhaps you should wait until you read the workshop about interest-bearing accounts like this.

If you found this workshop rewarding, please let us know on @(the-community-link)!

If you'd like to make this application a little more interesting, maybe you'd like to have a secret password just like @seclink["workshop-hash-lock"]{the hash lock} as well, so the Funder can separate the revealing of information to Receiver.
Similarly, you could make it like @seclink["workshop-relay"]{a relay account} and have the Receiver generated by the Funder and allow the Receiver to specify a third-party (fourth-party) to receive the actual funds.

We recommend that you take a pause from workshops like this and revisit the @emph{Rock, Paper, Scissors!} application in the @seclink["workshop-rps-fair"]{fairness workshop}.
Why don't you check it out?
