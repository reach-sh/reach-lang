#lang scribble/manual
@(require "lib.rkt")

@title[
  #:version reach-vers
  #:tag     "guide-rpc-server-api"
  ]{The Reach RPC server API and how to use it}

The Reach RPC server's design is deliberately lean and simple. In fact, it
mostly just re-exports everything offered by the standard library via HTTPS
under a corresponding @urlin{/stdlib/<function name>} route. Arguments and
their order remain unchanged.

@;TODO better example(?)
For example, rather than leveraging the standard library directly like so:
@js{ const balance = stdlib.formatCurrency('100000000000000000000', 18); }

...an equivalent RPC request is issued, where the payload is a JSON-encoded
array which represents ordered arguments to the target function:
@js{ POST /stdlib/formatCurrency ['100000000000000000000', 18] }

Likewise, Reach RPC @italic{@tech{frontends} and client libraries} are
correspondingly lean and simple. Unless you're constructing a new client
library (@seclink["guide-rpc-connect"]{more on this later}) for a language
which doesn't yet have one available, you won't issue these RPC requests
directly; you'll instead send such requests via a thin wrapper which has
already been provided for you.

@py{
  # In this example we're using the Python RPC client library
  balance = rpc('/stdlib/formatCurrency', '100000000000000000000', 18)
}

@italic{The remainder of this section introduces concepts discretely and with
short code snippets. If you'd prefer to see fully-realized examples of
@tech{RPC} @tech{frontends} in their entirety, it's recommended to browse the
@urlin{client-<language>} directories @reachexlink["tut-6-rpc" "here"].}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "connecting"]{Connecting to the RPC server}

By convention, @tech{RPC} @tech{frontends} issue all requests and receive all
responses through only two functions.

The simpler of the two is named @pyin{rpc}. @pyin{rpc}'s first argument is the
route which represents a desired operation; the rest are forwarded to this
operation as @italic{its} arguments.

@py{
  starting_balance = rpc('/stdlib/parseCurrency', 10)
  acc_alice        = rpc('/stdlib/newTestAccount', starting_balance)
  acc_bob          = rpc('/stdlib/newTestAccount', starting_balance)
}

The second - and more involved - function will be called something like
@pyin{rpc_callbacks} or @jsin{rpcCallbacks} depending upon the naming
conventions of the host language (e.g. "snake_case" in Python vs. "camelCase"
in JavaScript).

As with @pyin{rpc}, @pyin{rpc_callbacks}' first argument is again a route
representing a desired operation, but the route specified will always begin
with @urlin{/backend} and the remainder of its arguments will represent a
@tech{participant interact interface}. See:
@secref[#:tag-prefixes '("non-stdlib")]{backend}.

@pyin{rpc} and @pyin{rpc_callbacks} will generally be obtained by invoking a
"make RPC client" function called @jsin{mkRPC}, or @pyin{mk_rpc} (etc)
depending upon the host language plus the library author's naming style.

@py{
  rpc, rpc_callbacks = mk_rpc()
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "data-serialization"]{Data encoding}

Data sent to the server through @jsin{rpc} and @jsin{rpcCallbacks} should be
automatically JSON-encoded by the @tech{RPC} client library you use - as a
@tech{frontend} author you generally won't need to do anything manually in
order to make this happen.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "id-system"]{RPC ID system}

Slightly more involved than simple currency manipulation queries are the routes
which exist under @urlin{/acc}, @urlin{/ctc}, @urlin{/backend}, @urlin{/kont},
and @urlin{/forget}, as well as RPC-wrapped standard library functions having
to do with @tech{account} or @tech{contract} types.

Requests to these routes require an additional layer of indirection since
they're concerned with datatypes which cannot be fully serialized, and,
consequently, cannot be transmitted over network connections directly.

Consider the @tech{account} type, which encapsulates both static data fields
(which are serializable) and instance-bound methods (which aren't) at runtime.
In the case of a @tech{frontend} receiving some JSON blob representing an
@tech{account}, and later POSTing that blob back to the server within the
payload of a request, the server will lose track of critical information bound
to the original @tech{account} instance stored in memory unless they're
reconciled with additional bookkeeping.

A similar problem arises in the case of continuations for which there are
intermediate @tech{interact} steps that must be processed before execution may
resume.

Rather than transmitting these types directly it's necessary to maintain tables
of relevant accounts, contracts, and in-flight continuations on the
server-side. When issuing requests referencing any of these, @tech{RPC}
@tech{frontends} are instead provided with unique, unguessable IDs in their
place.

The composition of such IDs has little bearing on one's code when they're
concerned with building an @tech{RPC} @tech{frontend} program. Nevertheless,
being able to recognize them is helpful.

@js{
  // Here's an example RPC ID:
  '27_848d510db2cbc3aed620e44912ab754f17598c35cd86a736'
}

In practice you'll only ever include IDs as references in request payloads;
there are no useful operations which can be performed on them at the
client-side, and they encode no useful information about the data they
represent.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag-prefix "non-stdlib" #:tag "routes"]{Non-stdlib routes}

As mentioned before, there are other parent RPC routes than just
@urlin{/stdlib}. These route namespaces have special significance as described
below.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "acc-deploy"]{/acc/deploy}

@margin-note{
Q: What happened to the @jsin{backend} argument when calling @tech{account}
methods via RPC?

A: The server handles this so clients don't have to!
}

The @urlin{/acc/deploy} route corresponds to a @jsin{deploy} operation on
@tech{accounts} when using the standard library directly.

@(define link-deploy-stdlib @reachexlink[
  #:ref "27fb2a9"
  #:loc (cons "11" "12")
  "maybe-send/index.mjs"
  "this code from the \"maybe-send\""])

@(define link-deploy-rpc @reachexlink[
  #:ref "1474d32"
  #:loc "97"
  "tut-6-rpc/client-py/index.py"
  "RPC code from the \"tut-6-rpc\""])

Contrast @link-deploy-stdlib example:
@js{
  console.log(`Alice will deploy the contract.`);
  const ctcAlice = alice.deploy(backend);
}

...with the equivalent @link-deploy-rpc Python example:
@py{
  ctc_alice = rpc('/acc/deploy', acc_alice)
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "acc-attach"]{/acc/attach}

The @urlin{/acc/attach} route corresponds to an @jsin{attach} operation on
@tech{accounts} when using the standard library directly.

@(define link-attach-stdlib @reachexlink[
  #:ref "27fb2a9"
  #:loc (cons "14" "15")
  "maybe-send/index.mjs"
  "this code from the \"maybe-send\""])

@(define link-attach-rpc @reachexlink[
  #:ref "1474d32"
  #:loc "98"
  "tut-6-rpc/client-py/index.py"
  "RPC code from the \"tut-6-rpc\""])

Contrast @link-attach-stdlib example:
@js{
  console.log(`Bob will attach to the contract.`);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());
}

...with the equivalent @link-attach-rpc Python example:
@py{
  ctc_bob = rpc('/acc/attach', acc_bob, rpc('/ctc/getInfo', ctc_alice))
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "ctc-getinfo"]{/ctc/getInfo}

The @urlin{/ctc/getInfo} route corresponds to a @jsin{getInfo} operation on
@tech{contracts} when using the standard library directly.

Contrast @link-attach-stdlib example:
@js{
  console.log(`Bob will attach to the contract.`);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());
                                     ------------------
}

...with the equivalent @link-attach-rpc Python example:
@py{
  ctc_bob = rpc('/acc/attach', acc_bob, rpc('/ctc/getInfo', ctc_alice))
                                        ------------------------------
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "forget-acc"]{/forget/acc}
See: @secref{id-clean-up}.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "forget-ctc"]{/forget/ctc}
See: @secref{id-clean-up}.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "backend"]{/backend/*}

@(define link-backend-stdlib @reachexlink[
  #:ref "27fb2a9"
  #:loc (cons "41" "52")
  "tut-6/index.mjs"
  "this code from the (non-RPC) \"tut-6\""])

@(define link-backend-rpc @reachexlink[
  #:ref "1474d32"
  #:loc (cons "122" "144")
  "tut-6-rpc/client-py/index.py"
  "RPC code from the \"tut-6-rpc\""])

The @urlin{/backend} namespace is dynamically populated with routes which are
specific to your @|DApp|, e.g. @urlin{/backend/Alice}, and
@urlin{/backend/Bob}, or @urlin{/backend/SomeOtherParticipant} whose logic
you've encoded.

Contrast @link-backend-stdlib example:
@js{
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...Player('Alice'),
      wager: stdlib.parseCurrency(5),
    }),
    backend.Bob(ctcBob, {
      ...Player('Bob'),
      acceptWager: (amt) => {
        console.log(`Bob accepts the wager of ${fmt(amt)}.`);
      },
    }),
  ]);
}

...with the equivalent @link-backend-rpc Python example:
@py{
  def play_alice():
      rpc_callbacks(
          '/backend/Alice',
          ctc_alice,
          dict(wager=rpc('/stdlib/parseCurrency', 5), **player('Alice')))

  def play_bob():
      def acceptWager(amt):
          print('Bob accepts the wager of %s' % fmt(amt))

      rpc_callbacks(
          '/backend/Bob',
          ctc_bob,
          dict(acceptWager=acceptWager, **player('Bob')))

  alice = Thread(target=play_alice)
  bob   = Thread(target=play_bob)

  alice.start()
  bob.start()

  alice.join()
  bob.join()
}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@subsection[#:tag "kont"]{/kont}

Requests to @urlin{/kont} are automatically issued as necessary once a
@tech{participant} initiates a @urlin{/backend} exchange. These are normally
transparent to client authors unless they're
@seclink["guide-rpc-connect"]{constructing a new client library}.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@section[#:tag "id-clean-up"]{Relinquishing IDs once finished}

It's important that clients notify the server once they're done using a given
ID (lest the indexed data remain in memory needlessly until the server is
terminated). This is easily accomplished with requests to @urlin{/forget/acc}
and @urlin{/forget/ctc} at the appropriate point toward the end of your
client's lifespan.

@reachexlink[
  #:ref "1474d32"
  #:loc (cons "247" "248")
  "tut-6-rpc/client-go/index.go"
  "In go, for example:"]

@go{
  rpc("/forget/acc", accAlice, accBob)
  rpc("/forget/ctc", ctcAlice, ctcBob)
}

@;TODO * (?) Consult the @seclink["ref-rpc"]{RPC reference section} for more details.
@;TODO * (?) Consider using `reachex` for some of the larger code examples
@;TODO * (?) Mention envvars
@;TODO * (?) Explain TLS (development + production)
@;TODO * Margin notes don't float correctly on small screens/mobile devices
@;TODO * Flesh out /backend section + describe how `interact` hooks are provided
