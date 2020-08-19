#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "overview"]{Overview}

This is an informal overview of what Reach does and what the structure of a Reach program is.
The goal of this document is to give enough technical specifics to help you understand what Reach does, but it isn't intended as either a @seclink["tut"]{tutorial} or a @seclink["ref"]{reference}.
When you're ready to really begin a project, you can start with one of those, or a @seclink["howtos"]{how-to guide}.

@section[#:tag "over-model"]{Decentralized applications}

@DApps are made of multiple agents interacting with each other through some backend @tech{consensus network}, like Ethereum or Algorand.
These agents act on behalf of principals that provide direction and authority through information.
The consensus network allows these agents to transfer and receive value in the form of network-specific tokens, like ETH or ALGO.
The network also allows contracts to ensure that each agent follows the same rules as they take turns computing and publishing values and information.

A single Reach program incorporates all aspects of a @|DApp|:
@itemlist[

@item{@tech{Participant} @tech{backends} are the agents acting on behalf of the principals.}

@item{@tech{Frontends} are the technical representation of the interface between the @tech{participants} and the principals.}

@item{A @tech{contract} enforces the rules of the program, including the order of operation.}

]

In Reach, a programmer only needs to specify the actions of @tech{participants}---what they do individually and what they do in unison.
The Reach compiler automatically derives a @tech{contract} for the @tech{consensus network} via a @tech{connector} that enforces these rules.

@section[#:tag "over-minimal"]{A minial Reach program}

Let's look at a simple Reach program where two principals, Alice and Bob interact. In this @|DApp|, Alice has some information that Bob might want and she has an amount of network tokens in mind that she'd like to trade for it.

@margin-note{You can look at the entire example program by visiting @reachexlink["over-minimal.rsh"].}

The main part of the program looks like this:

@reachex[#:show-lines? #t "over-minimal.rsh"
         'skip 11 29 "      // ...body..."]

@itemlist[

@item{Line 1 specifies that this is a Reach program.}

@item{Line 3 defines the main @tech{export} from program. @litchar{main} is the default used by Reach.}

@item{Line 4 specifies that it is an application.}

@item{Lines 6 and 7 specify the interface between Alice's @tech{participant} and @tech{frontend}. In this case, Alice's @tech{frontend} must provide a number called @litchar{request} and a string called @litchar{info}.}

@item{Lines 8 and 9 specify the interface for Bob, which includes a function named @litchar{want}, that takes a number and returns @reachin{null}, as well as a function named @litchar{got}, that receives the information.}

@item{Finally, line 10, binds these @tech{participants} to the program identifiers @reachin{A} and @reachin{B}.}

]

The elided lines, 11 through 29, contain the body of the application, which we can divide into four parts.

@reachex[#:show-lines? #t "over-minimal.rsh"
         'only 11 14 "      // ..."]

@itemlist[

@item{Lines 11 and 12 specify that Alice takes a @tech{local step} where she @tech{declassifies} the amount of tokens requested.
In Reach, all values from the @tech{frontend} are @tech{secret} until explicitly made @tech{public} with @tech{declassify}.}

@item{Line 13 has Alice publish that value and the logic of the program transitions to specifying what the @tech{contract} does.}

@item{Line 14 has the @tech{contract} commit to these values and continue the rest of the program.}

]

At this point, Bob's @tech{backend} has learned the value of @reachin{request} and can deliver it to Bob's @tech{frontend} for his approval. This happens next.

@reachex[#:show-lines? #t "over-minimal.rsh"
         'only 16 19 "      // ..."]

@itemlist[

@item{Lines 16 and 17 has Bob perform that delivery.
@reachin{interact.want} doesn't explicitly return a boolean, because the frontend can not return if Bob doesn't want to continue.
A better version of this program might return @reachin{false} and have that communicated to Alice.}

@item{Lines 18 and 19 have Bob submit a payment matching the appropriate amount and then the @tech{contract} commits.}

]

It's now Alice's turn again,

@reachex[#:show-lines? #t "over-minimal.rsh"
         'only 21 25 "      // ..."]

@itemlist[

@item{Lines 21 and 22 specify that Alice @tech{declassifies} the information.}

@item{Line 23 has her publish it.}

@item{Line 24 has the @tech{contract} transfer the requested amount to her.}

@item{Line 25 commits the transactions on the @tech{consensus network}.}

]

The only thing left is for Bob's @tech{backend} to deliver the information to his @tech{frontend}.

@reachex[#:show-lines? #t "over-minimal.rsh"
         'only 27 29 "      // ..."]

@itemlist[

@item{Line 27 and 28 do this.}

@item{Line 29 exits the program.}

]

@section[#:tag "over-compile"]{Compile}

After a Reach programmer writes this application in a file like @reachexlink["over-minimal.rsh" @exec{over-minimal.rsh}], they could run

@commandline{reachc -o build @reachexlink["over-minimal.rsh" @exec{over-minimal.rsh}]}

and the @exec{build} directory will contain a new file named @reachexlink["build/over-minimal.main.mjs" @exec{over-minimal.main.mjs}], which contains a JavaScript implementation of a @tech{backend} for each participant, as well as the Ethereum bytecode for the @tech{contract}.

@margin-note{If you are curious, you can take a look at this file by going to @reachexlink["build/over-minimal.main.mjs"].
The Ethereum bytecode is not readable, but if you understand Solidity, you may want to look at @reachexlink["build/over-minimal.main.sol"] to see the original code that it was compiled from.}

For this 30 line application, the Reach compiler generated 51 lines of JavaScript code into two functions, one for Alice and one for Bob.
Separately, it generated 55 lines of Solidity code to implement the contract.
If a programmer wasn't using Reach, they would have to write each of these three modules separately and keep them synchronized at every step of the development process.
Morever, Reach doesn't only work for Ethereum, it is blockchain agnostic and can be easily configured to target other consensus networks, like Algorand.

@section[#:tag "over-verify"]{Verify}

Reach doesn't just compile your program, it also verifies it and ensures that entire categories of errors don't occur.
For example, it always guarantees that the balance in the @tech{contract} at the end of the program is zero.
This is important, because if it were not true, then tokens would be locked away by the @tech{contract} and inaccessible.

For this example program, it is obvious that when a single transfer of @reachin{request} goes in at line 18 and a single transfer of @reachin{request} goes out at line 24, then the balance is zero at the end of the program.
We could make a small tweak, however, to demonstrate things going wrong.

Let's change the third step to leave a single unit in the balance:

@reachex[#:show-lines? #t "over-minimal-error.rsh"
         'only 21 25 "      // ..."]

And then run the compiler

@commandline{reachc -o build @reachexlink["over-minimal-error.rsh" @exec{over-minimal-error.rsh}]}

It will print out a detailed error message showing the violation.

@reachex[#:show-lines? #t "over-minimal-error.txt"
         'only 1 11 "      // ..."]

Verification failures include a lot of information, such as a concrete counter-example showing values that could have been provided by @tech{frontends} that would lead to the property failing to hold.

There's a lot more to say about @seclink["guide-assert"]{automatic verification}; indeed, it is one of the main reasons programmers of decentralized applications want to use Reach, but we'll leave it at that for now.

@section[#:tag "over-interface"]{Interface}

XXX

@section[#:tag "over-execute"]{Execute}

XXX

@section[#:tag "over-next"]{Next steps}

In this overview, we've briefly described the structure and fundamental concepts of a Reach application.
We've shown how to construct a simple program, compile it, connect an interface, and run it.
Since this is only a brief overview of what Reach can do, we left a lot out.

Furthermore, this program has many flaws and should not be used in practice.
For example, it provides no protection to Bob in the event that Alice fails to deliver the information and makes no attempt to ensure that the information is what he wants.
Reach allows you to abstract away the low-level details of your decentralized program and focus on these sorts of bigger picture issues.
In the rest of @seclink["guide"]{the guide}, we discuss design issues like this. For example,

@itemlist[

@item{Performing @seclink["guide-assert"]{automatic verification} of your application; or}

@item{Fortifying your application against @seclink["guide-timeout"]{non-participation}.}

@item{Building @seclink["guide-abstract"]{interaction abstractions} for related applications.}

]

However, unless you're ready to dive deep now, the next steps for you are to:

@itemlist[

@item{@seclink["install"]{Install Reach};}

@item{Work through the @seclink["tut"]{tutorial}; and,}

@item{Join @(the-community-link).}

]

Thanks for being part of Reach!

