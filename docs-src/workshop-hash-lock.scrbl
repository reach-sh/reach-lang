#lang scribble/manual
@(require "lib.rkt")

@(define TAG "workshop-hash-lock")
@title[#:version reach-vers #:tag TAG]{Workshop: Hash Lock}

In this workshop, we'll design an application that allows a payer to lock funds with a secret password, independent from their @tech{consensus network} identity, which can be drawn by anyone possessing the secret password.
This is a useful way for a payer to show that they have funds and have committed to disbursing them, without deciding beforehand who they are paying.

@(workshop-deps)
@(workshop-init TAG)

@(drstep-pr TAG)

The first step in any program design is to perform problem analysis and determine what information is relevant to the problem.
When writting decentralized applications in Reach, this information analysis includes an analysis of the set of @tech{participants} involved in a computation.

In this case, let's ask the questions:
@itemlist[
 @item{Who is involved in this application?}
 @item{What information do they know at the start of the program?}
 @item{What information are they going to discover and use in the program?}
 @item{What funds change ownership during the application and how?}
]

You should write your answers in your Reach program (@tt{index.rsh}) using a comment. 
@reachin{/* Remember comments are written like this. */}

@(drstep-pr-stop)

Let's see how your answers compare to our answers:

@itemlist[
 @item{This program involves two parties: the payer sending the funds and the receiver of those funds.
By tradition, we'll call the first 'Alice' and the second 'Bob'.
You might like to use other names, like 'Sender' and 'Receiver'.}
 @item{Alice starts off knowing the amount she wants to send and the secret password.}
 @item{Bob starts off like Jon Snow and knows nothing.}
 @item{Alice doesn't learn anything during the execution of the program, but Bob learns the password.}
 @item{Alice transfers funds at the beginning of the program and Bob receives those funds at the end, after he learns the password.}
]

It's okay if your answers are different than ours.
Problem analysis is a "loose" process that is more like creative artistry than it is like rote calculation.
But, that doesn't mean it is superfluous and unnecessary or unneeded.

Problem analysis is a crucial step that helps us understand what our application is supposed to be doing.
Remember, programming in general, and Reach in particular, does not solve problems for you; instead, programs encode automatic solutions to problems you've already solved.
Compared to normal languages, Reach does do a bit automatically for you: it automatically discovers problems you may not have realized your program had.
You still have to solve them yourself though!
But, at least you know about them because of Reach.

@(drstep-dd TAG)

Humans and their social systems deal with information, but computers can only interact with data, which is merely a representation of information using particular structures, like numbers, arrays, and so on.
After problem analysis, we know what information our program will deal with, but next we need to decide how to translate that information into concrete data.

So, for this program, we should decide:
@itemlist[
 @item{What data type will represent the amount Alice transfers?}
 @item{What data type will represent Alice's password?}
]

After deciding those things, you should think about how the program will be provided this values.
In other words:

@itemlist[
 @item{What @tech{participant interact interface} will each participant use?}
]

You should look back at your problem analysis to do this step.
Whenever a participant starts of knowing something, then it is a field in the @reachin{interact} object.
If they learn something, then it will be an argument to a function.
If they provide something later, then it will be the result of a function.

You should write your answers in your Reach file (@tt{index.rsh}) as the @tech{participant interact interface} for each of the participants.

@(drstep-dd-stop)

Let's compare notes again.
@itemlist[
 @item{We're going to represent the amount Alice transfers as an unsigned integer (@reachin{UInt256}) named @reachin{amt}.}
 @item{We will represent the password as another unsigned integer (@reachin{UInt256}) named @reachin{pass}.}
 @item{These two values are the only fields of Alice's interface, but Bob will have a function named @reachin{getPass} that will return the password that he knows.}
]

We wrote this in our program as:

@reach{
  [['Alice', { amt : UInt256,
               pass: UInt256 }],
   ['Bob', { getPass: Fun([], UInt256) }] ],
}

It would be very surprising if you choose the exact same names as us in your code, but did you choose the same types?
We expect that many of you might have chosen to represent the password by a string of bytes using the Reach type, @reachin{Bytes}.
There's nothing necessarily wrong with this option, but we do not choose it because it is unnecessarily "wide".
For example, the empty string @reachin{''} is an example of a @reachin{Bytes} value, as is the text of this entire document and the combined works of Jane Austen.
The @reachin{Bytes} type contains many options, but we are likely to be satisfied with a mere 256-bits, or 32 characters, as represented by the @reachin{UInt256} type.
Have you ever had a password that was 32 characters long?

At this point, you can modify your JavaScript file (@tt{index.mjs}) to contain defintions of these values, although you may want to use a placeholder like @jsin{42} or something for the actual value.
When you're writing a Reach program, especially in the early phases, you should have these two files open side-by-side and update them in tandem as you're deciding the @tech{participant interact interface}.

@(drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication and transfer among the participants, including the consensus network.
For example, who initiates the application?
Who responds next?
Is there a repeated segment of the program that occurs over and over again?
We should explicitly write down this structure as comments in our program.
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

This is a simple application, so we should all share the same communication pattern.
Here's what we wrote:
@reach{
 // Alice pays the amount
 // Bob publishes the password
 // The consensus ensures it's the right password and pays Bob
}

However, looking at this pattern reveals a subtlety in this application:
how can the consensus ensure that Bob publishes the correct password?
The only way is for Alice to publish something first that can be checked by the consensus.
For example, we could use the pattern:
@reach{
 // Alice publishes the password and pays the amount
 // Bob publishes the password
 // The consensus ensures it's the right password and pays Bob
}

However, this is definitely wrong, because Alice doesn't want to share her password with the world across the network, she only wants to share it with Bob, potentially at some later moment.
So, she should publish the password, but instead, publish a @tech{digest} of the password, that can be checked against the actual password later.
In other words, we should use a pattern like:
@reach{
 // Alice publishes a digest of the password and pays the amount
 // Bob publishes the password
 // The consensus ensures it's the right password and pays Bob
}

It is cheaper to go through this iteration process in the human-centered design phase than in the code-centered programming phase, even when you're using a high-level language like Reach for programming.

Next, we need to convert this pattern into actual program code using @reachin{publish}, @reachin{pay}, and @reachin{commit}.

@(drstep-cc-stop2)

The body of your application should look something like:
@reach{
 // Alice publishes a digest of the password and pays the amount
 Alice.publish(passDigest, amt)
      .pay(amt);
 commit();

 // Bob publishes the password
 Bob.publish(pass);

 // The consensus ensures it's the right password and pays Bob
 transfer(amt).to(Bob);
 commit();
}

We can now move on to the next part of designing a decentralized application: verification.

@(drstep-ai TAG)

When we are programming, we hold a complex theory of the behavior of the program inside of our minds that helps us know what should happen next in the program based on what has happened before and what is true at every point in the program.
As programs become more complex, this theory becomes more and more difficult to grasp, so we might make mistakes.
Furthermore, when another programmer reads our code (such as a version of ourselves from the future trying to modify the program), it can be very difficult to understand this theory for ourselves.
@tech{Assert}ions are ways of encoding this theory directly into the text of the program in a way that will be checked by Reach and available to all future readers and editors of the code.

Look at your application, what are assumptions that you have about the values in the program?

@(drstep-ai-stop1)

There are three main assumptions we came up with for this program:
@itemlist[
 @item{Before Bob publishes the password, it is unknowable by him and everyone else except Alice.}
 @item{Bob assumes that the password digest published by Alice matches the digest of the password he's publishing.}
 @item{The consensus requires that Alice's digest and the digest of Bob's password match.}
]

We expect that the third of these is the least controversial and the most obvious property, but the others are important too.
The first property essentially guarantees that the errorneous version of the application we contemplated, where Alice directly sent her password over the network is disallowed.
The second property encodes Bob's assumption of good will and integrity when he submits his value: an honest version of the Bob participant would not willingly send a password that wasn't the correct one.
Furthermore, it is possible for any participant to check, without going through consensus, if they know what the password is.

Now that we know what the properties are, we need to encode them into our program via calls to Reach functions like @reachin{unknowable}, @reachin{assume}, and @reachin{require}.
Let's do that now.

@(drstep-ai-stop2)

Here's what we did:

@reach{
 // First
 unknowable(Bob, Alice(interact.pass));

 Bob.only(() => {
  // Second
  assume( passDigest == digest(pass) ); });
 Bob.publish(pass);

 // Third
 require( passDigest == digest(pass) );
 transfer(amt).to(Bob);
 commit();
}

@itemlist[
 @item{First, we assert that Bob can't know Alice's password, @emph{based on what the Reach program does}.}
 @item{Next, we assert that Bob believes his password is correct (and that an honest Bob will check it.)}
 @item{Finally, we assert that the consensus can only continue if this is the case.}
]

At this point, we are almost ready to complete our program and make it so that we can run it.
You've probably noticed that in our samples, the variables @reachin{pass}, @reachin{amt}, and @reachin{passDigest} are undefined.
We'll handle that next.

@(drstep-ii TAG)

A key concept of Reach programs is that they are concerned solely with the communication and consensus portions of a decentralized applications.
@tech{Frontends} are responsible for all other aspects of the program.
Thus, eventually a Reach programs needs to insert calls into their code to send data to and from the @tech{frontend} via the @tech{participant interact interface}s that they defined during the @emph{Data Definition} step.

In our program, that means defining @reachin{amt} and @reachin{passDigest} by Alice and @reachin{pass} by Bob.
Do that now.

@(drstep-ii-stop)

Here's what we did:

@reachex[#:show-lines? #t "workshop-hash-lock/index.rsh"
         #:link #t]

@itemlist[
 @item{Lines 10-12 have Alice declassify some of her values.}
 @item{Line 19 has Bob provide his password.}
]

@margin-note{Did you notice that we didn't mention what line 4 is for?
We'll discuss that in the next section; don't worry!}

At this point, when we

@cmd{../reach compile}

We'll get a happy message that all our theorems are true.
Great job!
But we still need to run our program!

@(drstep-de TAG)

At this point, we need to decide how we're going to deploy this program and really use it in the world.
We need to decide how to deploy the contract, as well as what kind of user interaction modality we'll implement inside of our @tech{frontend}.

@(drstep-de-stop)

In this case, it is a very simple program, so we'll use a simple and efficient contract @seclink["guide-deploymode"]{deployment mode}: @reachin{'firstMsg'}.
This means that the @tech{contract} won't exist on the @tech{consensus network} until Alice sends her first message.
This is a good choice for most contracts, if it is allowed.
(As @seclink["guide-deploymode"]{the guide discusses}, some applications cannot be deployed like this.}

@margin-note{Unfortunately, on many @tech{consensus networks}, like Ethereum and Algorand, this application is dangerous to run.
The problem is that a malicious miner, like Eve, can intercept Bob's message that provides him the funds, refuse to forward it through to the consensus network, take the password from it, and submit it for her own account.
There is not a good general solution to this problem, meaning a theorem that we could insert into our program to make sure this attack isn't possible, because the whole point of this application is that Bob's identity is not known at the time that Alice sends the first message.
Ideally, such networks would support a kind of cryptographic operation where Bob could prove that he knows the password without revealing it.
There are some ideas on how to provide this sort of thing through zero-knowledge proofs and homomorphic encryption, but there is no widely accepted and available solution.

In short: Don't run this program.
If you want to do something @emph{like this}, then continue to the @seclink["workshop-relay"]{next workshop } on relays.
If you want to do exactly this, then stay tuned for a more complex zero-knowledge version.}

Next, we'll settle for a simple testing program for now, to show that the application, and let the rest of our full stack team deal with actually building the interface.
Here's the JavaScript @tech{frontend} we wrote:

@reachex[#:show-lines? #t "workshop-hash-lock/index.mjs"
         #:link #t]

In this case, Bob learns the password outside of the Reach program by directly sharing memory with Alice.
In a real deployment, she might give Bob the password through some other channel, like an encrypted email message, or a calligraphic scroll delivered by raven or intoned from Himalayan cliffs.

With this testing frontend in place, we can run

@cmd{../reach run}

and see an example execution:

@verbatim{
$ ../reach run
Bob asked to give the preimage.
Returning: 40816662354916515903581596667174503941307255426903039386763272451578996162763
Alice went from 100.0 to 74.999999999999823944.
Bob went from 100.0 to 124.999999999999978599.
}

@section{Discussion}

You did it!

You implemented a Reach program totally on your own, with only a little bit of prodding.

Unlike @seclink["tut"]{the tutorial}, this workshop uses a "top-down" perspective on Reach application design, where you derive the program from the requirements and slowly fill out the shell, while knowing that each step was correct before moving on.
In contrast, in @seclink["tut"]{the tutorial}, we demonstrated a "bottom-up" style where you start implementing the easy parts and realize the problems and their fixes as you go.
There's no right way to program and in our own Reach development, we use a combination of the two tactics.
Try both and keep them both in mind during your own development.

If you found this workshop rewarding, please let us know on @(the-community-link)!

If you want to know what to do next, a natural extension of the concepts in this workshop is a @seclink["workshop-relay"]{relay account}.
Why don't you check it out?

