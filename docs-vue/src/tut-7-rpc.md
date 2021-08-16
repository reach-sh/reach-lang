


# {#tut-7-rpc} Rock, Paper, Scissors in Python





The main sequence of the tutorial uses the
[JavaScript frontend support library](##ref-frontends-js) to implement
a frontend for our Reach program using JavaScript.
But, Reach supports using any programming language through the
[Reach RPC Server](##ref-backends-rpc).

This tutorial walks through using this technique to implement a _Rock, Paper, Scissors!_ frontend
in Python.
It is based on the frontend from XXX (secref "tut-7"), so it does not include a
text-based interface, or a Web interface, but uses the final version of the
Reach code.

Below we will compare the XXX (secref "tut-7") JavaScript frontend with the
equivalent Python code communicating via RPC, section by section.
Follow along by typing the Python code into a file called `index.py`.

---

We begin by comparing the necessary imports and program body:

@[code{1-5}](@reach-lang/examples/js-impl)
@[code{1-10}](@reach-lang/examples/py-impl)

Rather than importing `loadStdlib` and `backend` as with the
JavaScript version, the Python frontend instead plucks `mk_rpc` from its
supporting `reach_rpc` library.
It is unnecessary for an RPC _frontend_ to import a backend
because the [RPC Server](##ref-backends-rpc) handles doing so instead.

The Python version also borrows functionality from the `random` and
`threading` libraries.
These will be necessary when providing callable methods in the
participant interact interface it offers the RPC server.

On line 9 the Python program binds `rpc` and `rpc_callbacks` out of
`mk_rpc`.
These two functions are the only tools we will need to communicate with the
RPC server.
See XXX (secref "ref-frontends-rpc-py") for more details on how they work.


---

Next, we define our Alice and Bob accounts and pre-fund them each with a
starting balance of `10`.

@[code{6-9}](@reach-lang/examples/js-impl)
@[code{11-14}](@reach-lang/examples/py-impl)

Translating code which uses the
[JavaScript frontend support library](##ref-frontends-js) to its
[Python RPC](##ref-frontends-rpc-py) equivalent is a simple matter of
specifying the corresponding RPC method (e.g.
`'/stdlib/newTestAccount'`), and supplying the same arguments thereafter.

---
Now we define two helper functions and use them to query Alice and Bob's
beginning balances:

@[code{10-14}](@reach-lang/examples/js-impl)
@[code{15-23}](@reach-lang/examples/py-impl)

---

Deploying and attaching to contracts works slightly differently over RPC:

@[code{15-17}](@reach-lang/examples/js-impl)
@[code{24-26}](@reach-lang/examples/py-impl)

As previously mentioned, it is the responsibility of the
[RPC Server](##ref-backends-rpc) (rather than that of the frontend
communicating over RPC) to interface with the DApp's backend, so that
argument is absent in the Python version shown above.
Instead, Alice's account RPC handle alone is sufficient for her to
deploy, and only Bob's account RPC handle and Alice's
contract RPC handle are necessary for Bob to attach.

---

`HAND` and `OUTCOME` only differ syntactically from their JavaScript
equivalents:

@[code{18-19}](@reach-lang/examples/js-impl)
@[code{27-29}](@reach-lang/examples/py-impl)

---

Even participant interact interface definitions remain largely the same:

@[code{20-32}](@reach-lang/examples/js-impl)
@[code{30-35}](@reach-lang/examples/py-impl)

Here, both the JavaScript and Python frontends begin declaring a
reusable "player constructor".
This constructor represents those fields which are common to both Alice and
Bob's participant interact interfaces.

The JavaScript code explicitly includes `...stdlib.hasRandom` itself, but
the Python code can instead direct the RPC server to append it to the interface
by including `'stdlib.hasRandom': True` as a field in the constructor's
[return value](#py-return).

Next, they each define a `getHand` function which randomly selects an
element from the previously defined `HAND` set and returns it to the
backend.
This function will be passed as a callable method of the interface later.

The Python version does not mimic the JavaScript's occasional "pause behavior",
although it easily could with a few extra lines of code.

---

`informTimeout` requires no subsequent backend interaction and is
accordingly easily to implement in either language:

@[code{36-38}](@reach-lang/examples/js-impl)
@[code{36-38}](@reach-lang/examples/py-impl)

---

<a name="py-return"></a>
The same is true of `seeOutcome`:

@[code{33-35}](@reach-lang/examples/js-impl)
@[code{39-48}](@reach-lang/examples/py-impl)

At the end of the Python code we return a `dict` that represents those
fields which are common to both Alice and Bob's
participant interact interfaces.

Again, `'stdlib.hasRandom': True` has special significance when
communicating via RPC: it instructs the server to append this signature on the
receiving end.

---

Finally, we proceed to the most interesting part of the program and use the
code we have built up thus far to actually play a game of _Rock, Paper, Scissors!_:

@[code{41-60}](@reach-lang/examples/js-impl)
@[code{49-84}](@reach-lang/examples/py-impl)

In the Python version we create a function called `play_alice` and spawn
it as a concurrent thread, which begins running in the background on line 56.

`play_alice` sends Alice's contract RPC handle and her
participant interact interface to the server with `rpc_callbacks`.
The interface includes methods and values created by `player('Alice')`,
and adds an additional `wager` value which is set to the result of
`rpc('/stdlib/parseCurrency', 5)`,
as well as setting a `deadline` of `10`.

Bob's interface is likewise defined and spawned as another thread, which also
begins running concurrently on line 68.
In Bob's case we add an `acceptWager` method instead of another value to
his participant interact interface.

Calling `.join()` on `alice` and `bob` instructs the main thread
to wait until both child threads have run to completion, signifying the end of
the _Rock, Paper, Scissors!_ game.
At this point we again collect each player's remaining balance and print them
to the console.
Each player's child thread will have already printed their success/failure
result to the screen prior to reaching this step, because that is how we encoded
their `seeOutcome` methods.

All that remains is to release Alice and Bob's RPC handles from the
server's memory on lines 79 and 80 with the `/forget/acc` and
`/forget/ctc` methods, then instruct the Python process' interpreter to
invoke our `main` function.

---

Now that we have written an entire _Rock, Paper, Scissors!_ game in Python it is time to try
running it.

First you will need to copy the `index.rsh` file you used for [the tutorial](##tut-7)
into the directory where you saved `index.py`.

Next, open a terminal in that directory and install the Reach Python RPC
client:
```
$ ([ -d ./venv ] || python3 -m venv ./venv) && source ./venv/bin/activate
```

::: note
What is this `"venv"` thing?

A Python
[venv](https://packaging.python.org/guides/installing-using-pip-and-virtual-environments/#creating-a-virtual-environment)
is a "virtual environment" that sandboxes dependencies to avoid cluttering your
system directories.
:::
```
$ pip install --upgrade reach-rpc-client
```


Then use `./reach rpc-run` to play a game of _Rock, Paper, Scissors!_:
```
$ ./reach rpc-run python3 -u ./index.py
```


::: note
Consult the [command-line](##ref-usage-rpc-run) reference section for more details on
how this sub-command works.
:::

Its output will be the same as the [final tutorial](##tut-7) version of the frontend:

```
Bob accepts the wager of 5
Alice played Rock
Bob played Paper
Bob saw outcome Bob wins
Alice saw outcome Bob wins
Alice went from 10 to 4.9999
  Bob went from 10 to 14.9999
```


This will launch an RPC server using the development API key
`"opensesame"` and a TLS certificate designed for testing.
::: note
Deploying your DApp into production with the RPC server requires obtaining a
certificate which is specific to your DNS domain and which has been signed by a
certificate authority such as
[Let's Encrypt](https://letsencrypt.org/getting-started/).

Users who are ready to go live should consult the [RPC Server command-line](##ref-usage-rpc-server)
reference section for configuration details.
:::

When you are done, type `deactivate` to exit your `venv`.

Well done! You have just reimplemented [the tutorial](##tut-7) in Python.

---

This tutorial uses Python to demonstrate how RPC frontends are
built in Reach, but it is similarly easy to write RPC frontends in other
languages, such as with the XXX (secref "ref-frontends-rpc-js") and
XXX (secref "ref-frontends-rpc-go") libraries.
