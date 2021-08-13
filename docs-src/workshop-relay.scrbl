#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-relay")
@title[#:version reach-vers #:tag TAG]{Workshop: Relay Account}

In this workshop, we'll revisit the problem of allowing a payer to transfer funds to another party before knowing their identity.
However, unlike in @secref["workshop-hash-lock"], we will use a technique that is safe against malicious miners.
One deployment of a decentralized application like this is as a "gift card" where a funder provides a fixed amount of currency to another without knowing their identity.

@(workshop-deps "workshop-hash-lock")
@(workshop-init TAG)

@(drstep-pr TAG)

For this workshop, we'll provide some constraints on your solution and problem analysis, since we'd like you to explore writing a Reach program with a specific design.

The overall purpose of this application is so that:
@itemlist[
 @item{Alice can decide an amount of funds to provide.}
 @item{Alice later decides who will have access to this by sharing a secret with them.
  We call this person, Bob.}
 @item{Bob can transfer the funds to wherever he'd like.}
]

In @secref["workshop-hash-lock"], we designed the application so that the "secret" was a special number that the @tech{contract} compared against a known @tech{digest} to release the funds.
This approach was flawed, because when Bob used the secret to gain access, it was possible for anyone else to see the transaction and attempt to play it themselves.

In today's workshop, we'll use a crucial insight about decentralized applications: account ownership is fluid and account credentials are a form of secret knowledge that every @tech{consensus network} builds in to their foundation.
With that in mind, let's use the following design:

@centered{Alice will represent her secret as an account that is authorized to withdraw the funds.}

This is called a @deftech{relay account}, because it exists temporarily to faciliate the relaying of funds from Alice to Bob.

With this in mind, let's answer the questions:
@itemlist[
 @item{Who are the principals of the application?}
 @item{What are the participants of the program?}
 @item{What information do they know at the start of the program?}
 @item{What information are they going to discover and use in the program?}
 @item{What funds change ownership during the application and how?}
]

@(drstep-pr-stop)

Let's see how your answers compare to our answers:

@itemlist[
 @item{This application involves two principals: Alice, who sends funds, and Bob, who receives funds.}
 @item{The program has two participants: Alice, who initiates the application, and the Relay, which transfers the funds to Bob.}
 @item{Alice starts knowing the amount she wants to transfer.}
 @item{Alice creates the Relay account, while the Relay account learns the address of Bob, who will receive the funds.}
 @item{The funds start with Alice and then move to Bob under the instruction of the Relay.}
]

The most surprising thing about this application is that Bob is not one of the participants in the application!
Of course, the Relay will actually run under the auspices of Bob, after Alice shares the account credentials with him, but there is a distinction in the program between Bob's identity and the Relay's.

@(drstep-dd TAG)

The next step of designing our program is representing this information in our program and deciding the @tech{participant interact interface} for each participant.
Which pieces of information go with which participants?
Which are functions and which are values?
Finally, how should the Relay account information and Bob's identity be represented?
(Hint: Reach has a type named @reachin{Address} that represents an account @tech{address}!)
@(drstep-dd-datatype-mn)

@(drstep-dd-stop)

Let's compare notes again.
Here's what we wrote in our program:

@reachex["workshop-relay/index.rsh"
         
         'only 6 8 "  // ..."]

We chose to represent the amount as a @reachin{UInt} field, which should be unsurprising.
We then have two functions that take no arguments and return an @reachin{Address} which respectively return the Relay identity and the Bob identity.
The idea here is that Alice will create the Relay account in the midst of the program and Bob will provide his own identity when he's acting as Relay.

@(drstep-cc TAG)

Now, we can write down the structure of communication and action in our application.
Try this on your own based on your experience with @secref["workshop-hash-lock"].

@(drstep-cc-stop1)

Here's what we wrote:
@reach{
 // 1. Alice pays the amount and says who the Relay is.
 // 2. The consensus remembers who the Relay is.
 // 3. The Relay publishes who Bob is.
 // 4. The consensus pays Bob.
}

We assume that most of you found it natural to think of steps one, three, and four, but found step two to be a strange addition.
Perhaps you felt that step two is implied by step one, where Alice says who the Relay is.
But, it all depends upon what the meaning of the word "is" is.
Since that is unclear to some, we'll make it explicit by stating that the consensus will remember the Relay's identity.

The next step is to convert this pattern into actual program code using @reachin{publish}, @reachin{pay}, and @reachin{commit}.
However, we expect that you'll need a bit of help with step two.
Reach has a special operation, available only in @tech{consensus steps}, for asserting the identity of a participant: @reachin{Participant.set}.
You can write @reachin{Relay.set(someAddr)} to assert that the address of the Relay is @reachin{someAddr}.
With that in mind...

@(drstep-cc-stop2)

The body of your application should look something like:
@reach{
  // 1. Alice pays the amount and says who the Relay is.
  Alice.publish(amt, relay)
    .pay(amt);

  // 2. The consensus remembers who the Relay is.
  Relay.set(relay);
  commit();

  // 3. The Relay publishes who Bob is.
  Relay.publish(bob);

  // 4. The consensus pays Bob.
  transfer(amt).to(bob);
  commit();
}

We expect that for most of you, the coding of step four is also a bit strange, because we've never seen an example where the destination of a transfer is not a @tech{participant}.
You may have thought that the @reachin{to} position in a @reachin{transfer} must be a participant, but actually it can be any address.
Participants, however, can be used as addresses if they are bound.

@margin-note{You might like to re-write this program to have a third participant, Bob, who takes no actions, and try to write @reachin{transfer(amt).to(Bob)}.
You'll find that Reach rejects this program because Bob is not bound.
You can correct this by adding @reachin{Bob.set(bob)} after the Relay publishes Bob's address.
There's nothing better about this version of the program, but it is unneccessary to have a participant like Bob that performs no part in the computation.}

@(drstep-ai TAG)

As usual, we should consider what assertions we can add to our program.
In some ways this is what we just did with the @reachin{Relay.set(relay)} line above, but that is unlike a normal assertion in that it is added primarily to direct the runtime activities on the consensus @tech{contract}, rather than as a statement about the logical properties of our program variables.

Sometimes it can be difficult to decide which things are @emph{part of} the application, like this, and which things are @emph{properties of} the application, like the assertions we've seen before.
This is a general problem in verification where the logical properties of the @emph{desired} program are often mixed up with the logical properties of the @emph{actual} program.
If you're interested in this topic, you might like to spend time reading about @link["https://en.wikipedia.org/wiki/Formal_specification"]{formal specification on Wikipedia}.

Now, if we were devious, we might send you on SNARK hunt after some more assertions to add to our program.
But, we're not mean any more, so we'll just tell you that there's nothing else to assert about this program.

@(drstep-ii TAG)

Next, we need to insert the appropriate calls to @reachin{interact}.
In this case, our program is very simple and we expect you'll do a great job without further discussion.

@(drstep-ii-stop)

Let's look at our whole program now:

@reachex["workshop-relay/index.rsh"
         ]

@(drstep-de TAG)

This program is a bit odd to test, because it relies on Alice creating a temporary account and then sharing its information with Bob.
We don't know of any beautiful way to derive this program from first principles, and instead must appeal to your JavaScript programming skills.
If you'd like a hint, remember that you can call @jsin{stdlib.newTestAccount} any number of times and that a @tech{backend}'s participant functions don't need to be called at the same time.

If you're brave, then try it yourself; otherwise, scroll down to see our solution.

@(drstep-de-stop)

Here's the JavaScript @tech{frontend} we wrote:

@reachex["workshop-relay/index.mjs"
         ]

We do a few sneaky things in this program:
@itemlist[
 @item{Lines 18 through 21 create a JavaScript @link["https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise"]{Promise} that will be filled in later by Alice.}
 @item{Alice's @reachin{getRelay} function (lines 26 through 32) creates the new account and communicates it "outside of the network" through the aforementioned Promise.}
 @item{Bob's thread (lines 34 through 48) waits for the Promise to resolve and then connects to the application with this new account.}
 @item{The Relay's @reachin{getBob} function (lines 43 through 46) returns his own address to receive the funds.}
]

If this program is scary for you, don't worry!
It uses some fairly esoteric JavaScript features to make a completely automated test of this program.
If instead you wrote it so that it ran interactively and had Bob paste in the information about the new Relay account, it might be easier for you to code with those two aspects totally separated.

Let's see what it looks like when we run this program:

@verbatim{
$ ../reach run
Alice creates a Relay account.
Bob waits for Alice to give him the information about the Relay account.
Alice shares it with Bob off chain.
Bob deposits some funds into the Relay to use it
Bob attaches to the contract as the Relay.
Bob joins the application as the Relay.
Bob, acting as the Relay, gives his information.
Alice went from 100.0 to 74.999999999999804065.
Bob went from 100.0 to 123.999999999999979.
}

@section[#:tag (format "~a-dns" TAG)]{Discussion and Next Steps}

Great job!
You could use this application today and start minting gift cards of tokens for your friends on their birthdays!
Wouldn't that be fun?

If you found this workshop rewarding, please let us know on @(the-community-link)!

If you'd like to make this application a little more interesting, maybe you'd like to have a secret password just like @seclink["workshop-hash-lock"]{the hash lock} as well, so Alice can separate the revealing of information to Bob.

If you want to know what to do next to advance your study of decentralized application design, a natural extension of the concepts in this workshop is a @seclink["workshop-trust-fund"]{trust fund}.
Why don't you check it out?
