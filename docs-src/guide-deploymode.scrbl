#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-deploymode"]{Choosing a deployment mode}

@reachin{Reach.App} takes an option named @reachin{deployMode} (as described @seclink["ref-programs-reach.app"]{in the reference}) that determines how the @tech{contract} of a Reach application should be @tech{deployed}.

The options are:

@bold{Constructor (@reachin{'constructor'}).}
In this mode, the contract is deployed independently of all of the @tech{participants} and available to the public to connect to.
This is the most flexible option, because it imposes no restrictions on the body of the application.
It has the downside of being slightly more expensive, because the construction of the contract must be paid for independently of the first use of the contract.
Furthermore, the creation of the contract is public, so it is possible for an agent @litchar{X} to create a contract @litchar{F} and intend to play the role of participant @litchar{A} in it, but the creation of the contract is observed by a third-party, @litchar{Z}, who interacts with @litchar{F} before @litchar{X} and acts as @litchar{A}.
This is particularly nefarious if @litchar{X} has already shared the information about the contract with their intended counter-party, @litchar{Y}.

@(define deadlock @link["https://en.wikipedia.org/wiki/Deadlock"]{deadlock})

@bold{Delayed (@reachin{'firstMsg'}).}
In this mode, the contract is deployed at the time of the first publication in the application.
This is slightly more efficient, because the contract creation is bundled with the first application.
This means that the contract does not exist until after the application starts running, which means that some features, like @reachin{wait} and @reachin{.timeout}, as well as collective operations, are not available until after the first action.
Furthermore, it complicates the sharing of information about a contract by the deployer with a third-party, because the deployer must introduce an @reachin{interact} method to transfer control to the @reachin{frontend} which will call (e.g.) @jsin{await ctc.getInfo()} and extract the information about the program.
If the @tech{frontend} attempts to call @jsin{await ctc.getInfo()} too early, it will @|deadlock|.

For example, consider the @tech{frontend} in the seventh version of the @emph{Rock, Paper, Scissors!} tutorial:

@reachex[#:mode js
         #:show-lines? #t "tut-7/index.mjs"
         #:link #t
         'only 23 33 "  // ..."]

@itemlist[

@item{Line 26 instructs the backend that it is the deployer.}

@item{Line 27 immediately reads the contract information.}

]

If @reachexlink["tut-7/index.rsh"] were defined to use @reachin{deployMode} @reachin{'firstMsg'}, then this call would @|deadlock|, because the contract information would not yet be available.
(Furthermore, the whole premise of this code, where on line 24, the user is asked if they will deploy, is unnecessary, because Alice would @emph{always} deploy.
The code for the non-deploy case, line 33, would move to exclusively occur on Bob's branch.)
Instead, the Reach application would need to introduce a new @reachin{interact} method called by Alice @emph{after} the first message to observe the contract information. 

@reachex[#:show-lines? #t "tut-7/index.rsh"
         #:link #t
         'only 42 48 "    // ..."]

This method would be called on line 47 in a new @reachin{only} block.

This complexity is why @reachin{'firstMsg'} is not the default, despite being more efficient.

@bold{Factory (@reachin{'factory'}).}
A future version of Reach will support a "factory" deployment mode where the contract is independently deployed into a meta-contract with a single method that creates new instances of its child contract.
This style of deployment has the same restrictions as the (@reachin{'firstMsg'}) style, but can be even more efficient if a contract family is used many times.
Furthermore, the contract factory may emit events that can be used to more easily observe the creation of new contract (i.e. application) instances.
