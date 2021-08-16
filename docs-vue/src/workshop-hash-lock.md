



# {#TAG} Workshop: Hash Lock

In this workshop, we'll design an application that allows a payer to lock funds with a secret password, independent from their consensus network identity, which can be drawn by anyone possessing the secret password.
This is a useful way for a payer to show that they have funds and have committed to disbursing them, without deciding beforehand who they are paying.

XXX (workshop-deps)
XXX (workshop-init TAG)

XXX (drstep-pr TAG)

The first step in any program design is to perform problem analysis and determine what information is relevant to the problem.
When writing decentralized applications in Reach, this information analysis includes an analysis of the set of participants involved in a computation.

In this case, let's ask the questions:
+ Who is involved in this application?
+ What information do they know at the start of the program?
+ What information are they going to discover and use in the program?
+ What funds change ownership during the application and how?


You should write your answers in your Reach program (`index.rsh`) using a comment.
`/* Remember comments are written like this. */`

XXX (drstep-pr-stop)

Let's see how your answers compare to our answers:

+ This program involves two parties: the payer sending the funds and the receiver of those funds.
By tradition, we'll call the first 'Alice' and the second 'Bob'.
You might like to use other names, like 'Sender' and 'Receiver'.
+ Alice starts off knowing the amount she wants to send and the secret password.
+ Bob starts off like Jon Snow and knows nothing.
+ Alice doesn't learn anything during the execution of the program, but Bob learns the password.
+ Alice transfers funds at the beginning of the program and Bob receives those funds at the end, after he learns the password.


It's okay if your answers are different than ours.
Problem analysis is a "loose" process that is more like creative artistry than it is like rote calculation.
But, that doesn't mean it is superfluous and unnecessary or unneeded.

Problem analysis is a crucial step that helps us understand what our application is supposed to be doing.
Remember, programming in general, and Reach in particular, does not solve problems for you; instead, programs encode automatic solutions to problems you've already solved.
Compared to normal languages, Reach does do a bit automatically for you: it automatically discovers problems you may not have realized your program had.
You still have to solve them yourself though!
But, at least you know about them because of Reach.

XXX (drstep-dd TAG)

Humans and their social systems deal with information, but computers can only interact with data, which is merely a representation of information using particular structures, like numbers, arrays, and so on.
After problem analysis, we know what information our program will deal with, but next we need to decide how to translate that information into concrete data.

So, for this program, we should decide:
+ What data type will represent the amount Alice transfers?
+ What data type will represent Alice's password?

XXX (drstep-dd-datatype-mn)

After deciding those things, you should think about how the program will be provided this values.
In other words:

+ What participant interact interface will each participant use?


You should look back at your problem analysis to do this step.
Whenever a participant starts off knowing something, then it is a field in the `interact` object.
If they learn something, then it will be an argument to a function.
If they provide something later, then it will be the result of a function.

You should write your answers in your Reach file (`index.rsh`) as the participant interact interface for each of the participants.

XXX (drstep-dd-stop)

Let's compare notes again.
+ We're going to represent the amount Alice transfers as an unsigned integer (`UInt`) named `amt`.
+ We will represent the password as another unsigned integer (`UInt`) named `pass`.
+ These two values are the only fields of Alice's interface, but Bob will have a function named `getPass` that will return the password that he knows.


We wrote this in our program as:

```reach
[Participant('Alice', { amt : UInt,
             pass: UInt }),
 Participant('Bob', { getPass: Fun([], UInt) }) ],
```


It would be very surprising if you choose the exact same names as us in your code, but did you choose the same types?
We expect that many of you might have chosen to represent the password by a string of bytes using the Reach type, `Bytes`.
There's nothing necessarily wrong with this option, but we did not choose it because it is hard to decide exactly how long to make it, but we are satisfied with an unsigned integer, because it has a minimum of 64 bits on typical consensus networks.

At this point, you can modify your JavaScript file (`index.mjs`) to contain definitions of these values, although you may want to use a placeholder like `42` or something for the actual value.
When you're writing a Reach program, especially in the early phases, you should have these two files open side-by-side and update them in tandem as you're deciding the participant interact interface.

XXX (drstep-cc TAG)

A fundamental aspect of a decentralized application is the pattern of communication and transfer among the participants, including the consensus network.
For example, who initiates the application?
Who responds next?
Is there a repeated segment of the program that occurs over and over again?
We should explicitly write down this structure as comments in our program.
For example, for the [tutorial](##tut) version of _Rock, Paper, Scissors!_, we might write:
```reach
// Alice publishes the wager and pays it
// Bob accepts the wager and pays it
// While there's a draw
//  Alice publishes her hand secretly
//  Bob publishes his hand publicly
//  Alice reveals her hand
//  The consensus ensures it's the same hand as before
// The consensus pays out the wager
```


You should do this now, in your Reach program (`index.rsh`).

XXX (drstep-cc-stop1)

This is a simple application, so we should all share the same communication pattern.
Here's what we wrote:
```reach
// Alice pays the amount
// Bob publishes the password
// The consensus ensures it's the right password and pays Bob
```


However, looking at this pattern reveals a subtlety in this application:
how can the consensus ensure that Bob publishes the correct password?
The only way is for Alice to publish something first that can be checked by the consensus.
For example, we could use the pattern:
```reach
// Alice publishes the password and pays the amount
// Bob publishes the password
// The consensus ensures it's the right password and pays Bob
```


However, this is definitely wrong, because Alice doesn't want to share her password with the world across the network, she only wants to share it with Bob, potentially at some later moment.
So, she should not publish the password, but instead, publish a digest of the password, that can be checked against the actual password later.
In other words, we should use a pattern like:
```reach
// Alice publishes a digest of the password and pays the amount
// Bob publishes the password
// The consensus ensures it's the right password and pays Bob
```


It is cheaper to go through this iteration process in the human-centered design phase than in the code-centered programming phase, even when you're using a high-level language like Reach for programming.

Next, we need to convert this pattern into actual program code using `publish`, `pay`, and `commit`.

XXX (drstep-cc-stop2)

The body of your application should look something like:
```reach
// Alice publishes a digest of the password and pays the amount
Alice.publish(passDigest, amt)
     .pay(amt);
commit();

// Bob publishes the password
Bob.publish(pass);

// The consensus ensures it's the right password and pays Bob
transfer(amt).to(Bob);
commit();
```


We can now move on to the next part of designing a decentralized application: verification.

XXX (drstep-ai TAG)

When we are programming, we hold a complex theory of the behavior of the program inside of our minds that helps us know what should happen next in the program based on what has happened before and what is true at every point in the program.
As programs become more complex, this theory becomes more and more difficult to grasp, so we might make mistakes.
Furthermore, when another programmer reads our code (such as a version of ourselves from the future trying to modify the program), it can be very difficult to understand this theory for ourselves.
Assertions are ways of encoding this theory directly into the text of the program in a way that will be checked by Reach and available to all future readers and editors of the code.

Look at your application, what are assumptions that you have about the values in the program?

XXX (drstep-ai-stop1)

There are three main assumptions we came up with for this program:
+ Before Bob publishes the password, it is unknowable by him and everyone else except Alice.
+ Bob assumes that the password digest published by Alice matches the digest of the password he's publishing.
+ The consensus requires that Alice's digest and the digest of Bob's password match.


We expect that the third of these is the least controversial and the most obvious property, but the others are important too.
The first property essentially guarantees that the errorneous version of the application we contemplated, where Alice directly sent her password over the network is disallowed.
The second property encodes Bob's assumption of good will and integrity when he submits his value: an honest version of the Bob participant would not willingly send a password that wasn't the correct one.
Furthermore, it is possible for any participant to check, without going through consensus, if they know what the password is.

Now that we know what the properties are, we need to encode them into our program via calls to Reach functions like `unknowable`, `assume`, and `require`.
Let's do that now.

XXX (drstep-ai-stop2)

Here's what we did:

```reach
// First
unknowable(Bob, Alice(_pass));

Bob.only(() => {
 // Second
 assume( passDigest == digest(pass) ); });
Bob.publish(pass);

// Third
require( passDigest == digest(pass) );
transfer(amt).to(Bob);
commit();
```


+ First, we assert that Bob can't know Alice's password, _based on what the Reach program does_.
+ Next, we assert that Bob believes his password is correct (and that an honest Bob will check it.)
+ Finally, we assert that the consensus can only continue if this is the case.


At this point, we are almost ready to complete our program and make it so that we can run it.
You've probably noticed that in our samples, the variables `pass`, `amt`, and `passDigest` are undefined.
We'll handle that next.

XXX (drstep-ii TAG)

A key concept of Reach programs is that they are concerned solely with the communication and consensus portions of a decentralized applications.
Frontends are responsible for all other aspects of the program.
Thus, eventually a Reach programs needs to insert calls into their code to send data to and from the frontend via the participant interact interfaces that they defined during the _Data Definition_ step.

In our program, that means defining `amt` and `passDigest` by Alice and `pass` by Bob.
Do that now.

XXX (drstep-ii-stop)

Here's what we did:

@[code](@reach-lang/examples/workshop-hash-lock/index.rsh)

+ Lines 11-14 have Alice declassify some of her values.
+ Line 21 has Bob provide his password.


::: note
Did you notice that we didn't mention what line 5 is for?
We'll discuss that in the next section; don't worry!
:::

At this point, when we

```
$ ../reach compile
```


We'll get a happy message that all our theorems are true.
Great job!
But we still need to run our program!

XXX (drstep-de TAG)

At this point, we need to decide how we're going to deploy this program and really use it in the world.
We need to decide how to deploy the contract, as well as what kind of user interaction modality we'll implement inside of our frontend.

XXX (drstep-de-stop)

In this case, it is a very simple program, so we'll use a simple and efficient contract [deployment mode](##guide-deploymode): `'firstMsg'`.
This means that the contract won't exist on the consensus network until Alice sends her first message.
This is a good choice for most contracts, if it is allowed.
(As [the guide discusses](##guide-deploymode), some applications cannot be deployed like this.}

::: note
Unfortunately, on many consensus networks, like Ethereum and Algorand, this application is dangerous to run.
The problem is that a malicious miner, like Eve, can intercept Bob's message that provides him the funds, refuse to forward it through to the consensus network, take the password from it, and submit it for her own account.
There is not a good general solution to this problem, meaning a theorem that we could insert into our program to make sure this attack isn't possible, because the whole point of this application is that Bob's identity is not known at the time that Alice sends the first message.
Ideally, such networks would support a kind of cryptographic operation where Bob could prove that he knows the password without revealing it.
There are some ideas on how to provide this sort of thing through zero-knowledge proofs and homomorphic encryption, but there is no widely accepted and available solution.

In short: Don't run this program.
If you want to do something _like this_, then continue to the [next workshop ](##workshop-relay) on relays.
If you want to do exactly this, then stay tuned for a more complex zero-knowledge version.
:::

Next, we'll settle for a simple testing program for now, to show that the application, and let the rest of our full stack team deal with actually building the interface.
Here's the JavaScript frontend we wrote:

@[code](@reach-lang/examples/workshop-hash-lock/index.mjs)

In this case, Bob learns the password outside of the Reach program by directly sharing memory with Alice.
In a real deployment, she might give Bob the password through some other channel, like an encrypted email message, or a calligraphic scroll delivered by raven or intoned from Himalayan cliffs.

With this testing frontend in place, we can run

```
$ ../reach run
```


and see an example execution:

```
$ ../reach run
Bob asked to give the preimage.
Returning: 40816662354916515903581596667174503941307255426903039386763272451578996162763
Alice went from 100.0 to 74.999999999999823944.
Bob went from 100.0 to 124.999999999999978599.
```


## {#(format ~a-dns TAG)} Discussion

You did it!

You implemented a Reach program totally on your own, with only a little bit of prodding.

Unlike [the tutorial](##tut), this workshop uses a "top-down" perspective on Reach application design, where you derive the program from the requirements and slowly fill out the shell, while knowing that each step was correct before moving on.
In contrast, in [the tutorial](##tut), we demonstrated a "bottom-up" style where you start implementing the easy parts and realize the problems and their fixes as you go.
There's no right way to program and in our own Reach development, we use a combination of the two tactics.
Try both and keep them both in mind during your own development.

If you found this workshop rewarding, please let us know on <CommunityLink />!

If you want to know what to do next, a natural extension of the concepts in this workshop is a [relay account](##workshop-relay).
Why don't you check it out?

