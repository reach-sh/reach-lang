#lang scribble/manual
@(require scribble/core
          scribble/html-properties
          "lib.rkt")

@title[#:version reach-vers #:tag "tut-7-rpc" #:style 'toc]{Rock, Paper, Scissors in Python}
@author[(author+email "Matt Audesse" "matt@reach.sh")]

@(define py-impl "tut-7-rpc/client-py/index.py")
@(define js-impl "tut-7/index.mjs")

The main sequence of the tutorial uses the
@seclink["ref-frontends-js"]{JavaScript frontend support library} to implement
a frontend for our Reach program using JavaScript.
But, Reach supports using any programming language through the
@seclink["ref-backends-rpc"]{Reach RPC Server}.

This tutorial walks through using this technique to implement a @|RPS| frontend
in Python.
It is based on the frontend from @secref["tut-7"], so it does not include a
text-based interface, or a Web interface, but uses the final version of the
Reach code.

Below we will compare the @secref["tut-7"] JavaScript @tech{frontend} with the
equivalent Python code communicating via RPC, section by section.

@(hrule)

First we need to set up our project and install the library from the command
line:

@verbatim{
$ mkdir rps-python
$ cd rps-python

$ printf 'venv/\nbuild/\n' > .gitignore

$ curl -o index.rsh "https://raw.githubusercontent.com/reach-sh/reach-lang/master/examples/tut-7-rpc/server/index.rsh"

$ curl -o reach "https://raw.githubusercontent.com/reach-sh/reach-lang/master/reach"

$ chmod +x ./reach

$ git init
$ git add .gitignore index.rsh reach
$ git commit -m 'First commit'

$ python3 -m venv ./venv
$ . ./venv/bin/activate
$ pip install --upgrade reach-rpc-client
}

@(hrule)

Now we can focus on building the actual program.
Follow along by typing the Python code into a file called @tt{index.py} in the
@tt{rps-python} directory you just created.

We begin by comparing the necessary imports and program body:

@reachex[#:show-lines? #t js-impl #:link #t 'only 1 5  "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 1 10  "# ..."]

Rather than importing @jsin{loadStdlib} and @jsin{backend} as with the
JavaScript version, the Python frontend instead plucks @pyin{mk_rpc} from its
supporting @pyin{reach_rpc} library.
It is unnecessary for an RPC @italic{frontend} to import a @tech{backend}
because the @seclink{ref-backends-rpc} handles doing so instead.

The Python version also borrows functionality from the @pyin{random} and
@pyin{threading} libraries.
These will be necessary when providing callable methods in the
@tech{participant interact interface} it offers the RPC server.

On line 9 the Python program binds @pyin{rpc} and @pyin{rpc_callbacks} out of
@pyin{mk_rpc}.
These two functions are the only tools we will need to communicate with the
RPC server.
See @seclink{ref-frontends-rpc-py} for more details on how they work.


@(hrule)

Next, we define our Alice and Bob accounts and pre-fund them each with a
starting balance of @tt{10}.

@reachex[#:show-lines? #t js-impl #:link #t 'only 6  9  "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 11 14  "# ..."]

Translating code which uses the
@seclink["ref-frontends-js"]{JavaScript frontend support library} to its
@seclink["ref-frontends-rpc-py"]{Python RPC} equivalent is a simple matter of
specifying the corresponding RPC method (e.g.
@tt{'/stdlib/newTestAccount'}), and supplying the same arguments thereafter.

@(hrule)
Now we define two helper functions and use them to query Alice and Bob's
beginning balances:

@reachex[#:show-lines? #t js-impl #:link #t 'only 10 14 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 15 23  "# ..."]

@(hrule)

Deploying and attaching to contracts works slightly differently over RPC:

@reachex[#:show-lines? #t js-impl #:link #t 'only 15 17 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 24 26  "# ..."]

As previously mentioned, it is the responsibility of the
@seclink{ref-backends-rpc} (rather than that of the @tech{frontend}
communicating over RPC) to interface with the @|DApp|'s @tech{backend}, so that
argument is absent in the Python version shown above.
Instead, Alice's @tech{account} @tech{RPC handle} alone is sufficient for her to
deploy, and only Bob's @tech{account} @tech{RPC handle} and Alice's
@tech{contract} @tech{RPC handle} are necessary for Bob to attach.

@(hrule)

@pyin{HAND} and @pyin{OUTCOME} only differ syntactically from their JavaScript
equivalents:

@reachex[#:show-lines? #t js-impl #:link #t 'only 18 19 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 27 29  "# ..."]

@(hrule)

Even @tech{participant interact interface} definitions remain largely the same:

@reachex[#:show-lines? #t js-impl #:link #t 'only 20 32 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 30 35  "# ..."]

Here, both the JavaScript and Python @tech{frontends} begin declaring a
reusable "player constructor".
This constructor represents those fields which are common to both Alice and
Bob's @tech{participant interact interfaces}.

The JavaScript code explicitly includes @jsin{...stdlib.hasRandom} itself, but
the Python code can instead direct the RPC server to append it to the interface
by including @pyin{'stdlib.hasRandom': True} as a field in the constructor's
@link["#py-return"]{return value}.

Next, they each define a @pyin{getHand} function which randomly selects an
element from the previously defined @pyin{HAND} set and returns it to the
@tech{backend}.
This function will be passed as a callable method of the interface later.

The Python version does not mimic the JavaScript's occasional "pause behavior",
although it easily could with a few extra lines of code.

@(hrule)

@pyin{informTimeout} requires no subsequent @tech{backend} interaction and is
accordingly easily to implement in either language:

@reachex[#:show-lines? #t js-impl #:link #t 'only 36 38 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 36 38  "# ..."]

@(hrule)

@(element (make-style #f (list (url-anchor "py-return"))) '())
The same is true of @pyin{seeOutcome}:

@reachex[#:show-lines? #t js-impl #:link #t 'only 33 35 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 39 48  "# ..."]

At the end of the Python code we return a @pyin{dict} that represents those
fields which are common to both Alice and Bob's
@tech{participant interact interfaces}.

Again, @pyin{'stdlib.hasRandom': True} has special significance when
communicating via RPC: it instructs the server to append this signature on the
receiving end.

@(hrule)

Finally, we proceed to the most interesting part of the program and use the
code we have built up thus far to actually play a game of @|RPS|:

@reachex[#:show-lines? #t js-impl #:link #t 'only 41 60 "// ..."]
@reachex[#:show-lines? #t py-impl #:link #t 'only 49 84  "# ..."]

In the Python version we create a function called @pyin{play_alice} and spawn
it as a concurrent thread, which begins running in the background on line 56.

@pyin{play_alice} sends Alice's @tech{contract} @tech{RPC handle} and her
@tech{participant interact interface} to the server with @pyin{rpc_callbacks}.
The interface includes methods and values created by @pyin{player('Alice')},
and adds an additional @pyin{wager} value which is set to the result of
@pyin{rpc('/stdlib/parseCurrency', 5)}.

Bob's interface is likewise defined and spawned as another thread, which also
begins running concurrently on line 68.
In Bob's case we add an @pyin{acceptWager} method instead of another value to
his @tech{participant interact interface}.

Calling @pyin{.join()} on @pyin{alice} and @pyin{bob} instructs the main thread
to wait until both child threads have run to completion, signifying the end of
the @|RPS| game.
At this point we again collect each player's remaining balance and print them
to the console.
Each player's child thread will have already printed their success/failure
result to the screen prior to reaching this step, because that is how we encoded
their @pyin{seeOutcome} methods.

All that remains is to release Alice and Bob's @tech{RPC handles} from the
server's memory on lines 79 and 80 with the @tt{/forget/acc} and
@tt{/forget/ctc} methods, then instruct the Python process' interpreter to
invoke our @pyin{main} function.

@(hrule)

Now that we have written an entire @|RPS| game in Python it is time to try
running it.

First we need to generate TLS certificates.
Think up a pass phrase greater than four characters long, then open a shell in
your @tt{rps-python} directory and type @tt{./reach tls}:

@verbatim{
$ ./reach tls
*** Warning! Certificates generated by `reach tls` are only suitable ***
    for development purposes and must not be used in production!

Scaffolding auto-generated TLS certificate + key under ./tls directory...
(When prompted for a pass phrase, please enter between 4 and 1024 characters)

Generating a RSA private key
....................................................++++
.................++++
writing new private key to './tls/reach-server.key'
Enter PEM pass phrase:
Verifying - Enter PEM pass phrase:
-----
Signature ok
subject=CN = reach-server
Getting Private key
Enter pass phrase for ./tls/reach-server.key:
  ...done

When invoking `reach server` you MUST supply the pass phrase you just
used to generate your certificate as an environment variable, e.g.:

  $ REACH_RPC_TLS_PASSPHRASE=ENTER-PASSPHRASE-HERE \
    REACH_RPC_KEY=YOUR-PRESHARED-KEY \
    ./reach server

./tls
├── reach-server.crt
├── reach-server.csr
├── reach-server.ext
└── reach-server.key

0 directories, 4 files
}

@(hrule)

With those created, now we launch the server:

@verbatim{
$  REACH_RPC_TLS_PASSPHRASE=ENTER-PASSPHRASE-HERE \
   REACH_RPC_KEY=YOUR-PRESHARED-KEY \
   ./reach server

===============================
using rpc key:
REACH_RPC_KEY=YOUR-PRESHARED-KEY
===============================

Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
  Verifying when NO participants are honest
  Verifying when ONLY "Alice" is honest
  Verifying when ONLY "Bob" is honest
Checked 93 theorems; No failures!
Creating network "rps-python_default" with the default driver
Creating rps-python_ethereum-devnet_1 ... done

> @"@reach-sh/rpc-server@" app /app
> node --experimental-modules --unhandled-rejections=strict index.mjs

(node:26) ExperimentalWarning: The ESM module loader is experimental.
}

The server is now awaiting connections.

@(hrule)

Open another terminal in your @tt{rps-python} directory and type the following
to play a game of @|RPS|:

@margin-note{
Alternatively, if you would prefer to supply configuration as arguments to the
@pyin{mk_rpc} function at runtime instead of using environment variables, this
also works:

@verbatim{
rpc, rpc_callbacks = \
    mk_rpc(dict(
  host='127.0.0.1',
  port='3000',
  verify='0',
  timeout='7',
  key='YOUR-PRESHARED-KEY',
))
}


You may choose whichever best suits your use case.

Combining both methods is also possible, but settings supplied as arguments to
@pyin{mk_rpc} are given priority over settings defined as environment
variables.

Read @seclink{ref-backends-rpc-opts} for more details.
}

@verbatim{
$ . ./venv/bin/activate
$ REACH_RPC_SERVER=127.0.0.1 \
  REACH_RPC_PORT=3000 \
  REACH_RPC_KEY=YOUR-PRESHARED-KEY \
  REACH_RPC_TLS_REJECT_UNVERIFIED=0 \
  python3 ./index.py

*** Warning! TLS verification disabled! ***

 This is highly insecure in Real Life™ applications and must
 only be permitted under controlled conditions (such as
 during development).

Bob accepts the wager of 5
Alice played Rock
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999
  Bob went from 10 to 14.9999
}

You may replay as many times as you would like by pressing @tt{"up"} once then
@tt{"return"} on your keyboard in this terminal.

When you are done type @tt{"control+c"} and then @tt{./reach down} in the first
terminal.

@(hrule)

This tutorial uses Python to demonstrate how RPC @tech{frontends} are
built in Reach, but it is similarly easy to write RPC @tech{frontends} in other
languages, such as with the @seclink{ref-frontends-rpc-js} and
@seclink{ref-frontends-rpc-go} libraries.
