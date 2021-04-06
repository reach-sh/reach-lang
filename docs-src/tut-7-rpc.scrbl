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

First, we need to install the library from the command line:
@cmd{
  pip install --upgrade reach-rpc-client
}

Next, the necessary imports and program body:

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

The code we have just built is complete but still requires some configuration
before it may run successfully.
The necessary settings are outlined in @seclink{ref-backends-rpc-opts}, and by
convention there are two ways in which @tech{frontends} may supply them:

@itemlist[
@item{as a @pyin{dict} argument to @pyin{mk_rpc}}
@item{as shell environment variables}
]

You may choose whichever best suits your use case.
Combining both methods is also possible, but settings supplied as arguments to
@pyin{mk_rpc} are given priority over settings defined as environment
variables.

Returning to line 9 of the @tt{index.py} example above, we could instead run
the @tech{frontend} like so:

@py{
def main():
    rpc, rpc_callbacks = mk_rpc(dict(
        host    = 'some-host.lan',
        port    = '3000',
        verify  = '0', # `0` may only be used during development!
        timeout = '7',
        key     = 'USE-YOUR-OWN-PRESHARED-KEY',
    ))
}

Or, from the command line:

@verbatim{
$ REACH_RPC_SERVER=some-host.lan \
  REACH_RPC_PORT=3000 \
  REACH_RPC_TLS_REJECT_UNVERIFIED=0 \
  REACH_RPC_TIMEOUT=7 \
  REACH_RPC_KEY=USE-YOUR-OWN-PRESHARED-KEY \
  python index.py
}

Either way would result in the same runtime configuration.

A game of @|RPS| built with our Python code will look like this (edited for
legibility):

@shell{
*** Warning! TLS verification disabled! ***

 This is highly insecure in Real Life™ applications and must
 only be permitted under controlled conditions (such as
 during development).

RPC /stdlib/parseCurrency [10]
RPC /stdlib/parseCurrency [10]
  ==> {"type": "BigNumber", "hex": "0x8ac7230489e80000"}

RPC /stdlib/newTestAccount
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}]
RPC /stdlib/newTestAccount
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}]
  ==> "0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"

RPC /stdlib/newTestAccount
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}]
RPC /stdlib/newTestAccount
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}]
  ==> "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"

RPC /stdlib/balanceOf
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
RPC /stdlib/balanceOf
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
  ==> {"type": "BigNumber", "hex": "0x8ac7230489e80000"}

RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}, 4]
RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}, 4]
  ==> "10"

RPC /stdlib/balanceOf
      ["1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"]
RPC /stdlib/balanceOf
      ["1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"]
  ==> {"type": "BigNumber", "hex": "0x8ac7230489e80000"}

RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}, 4]
RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x8ac7230489e80000"}, 4]
  ==> "10"

RPC /acc/deploy
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
RPC /acc/deploy
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
  ==> "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"

RPC /ctc/getInfo
      ["0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"]
RPC /ctc/getInfo
      ["0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"]
  ==> { "address":         "0x9000E6747Be7403bDA8D380F5E453eCa068A720B"
      , "creation_block":  3
      , "creator":         "0xCEB4bD4b5bf49a2c6b560d66A7f3f0ee060F4521"
      , "transactionHash": "0x5c8b43ea5a01f58362ed6360e67bb09ac6507..."
      }

RPC /acc/attach
      [ "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"
      , { "address":         "0x9000E6747Be7403bDA8D380F5E453eCa068A720B"
        , "creation_block":  3
        , "creator":         "0xCEB4bD4b5bf49a2c6b560d66A7f3f0ee060F4521"
        , "transactionHash": "0x5c8b43ea5a01f58362ed6360e67bb09ac6507..."
        }
      ]
RPC /acc/attach
      [ "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"
      , { "address":         "0x9000E6747Be7403bDA8D380F5E453eCa068A720B"
        , "creation_block":  3
        , "creator":         "0xCEB4bD4b5bf49a2c6b560d66A7f3f0ee060F4521"
        , "transactionHash": "0x5c8b43ea5a01f58362ed6360e67bb09ac6507..."
        }
      ]
  ==> "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"

RPC /stdlib/parseCurrency [5]

RPC /backend/Bob
      [ "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"
      , { "stdlib.hasRandom": true }
      , { "acceptWager":      true
        , "getHand":          true
        , "informTimeout":    true
        , "seeOutcome":       true
        }
      ]

RPC /stdlib/parseCurrency [5]
  ==> {"type": "BigNumber", "hex": "0x4563918244f40000"}

RPC /backend/Alice
    [ "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"
    , { "wager": { "type": "BigNumber", "hex": "0x4563918244f40000" }
      , "stdlib.hasRandom": true
      }
    , { "getHand":          true
      , "informTimeout":    true
      , "seeOutcome":       true
      }
    ]


RPC /backend/Bob
      [ "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"
      , { "stdlib.hasRandom": true }
      , { "acceptWager":      true
        , "getHand":          true
        , "informTimeout":    true
        , "seeOutcome":       true
        }
      ]
  ==> { "t":    "Kont"
      , "kid":  "0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de"
      , "m":    "acceptWager"
      , "args": [{ "type": "BigNumber", "hex": "0x4563918244f40000" }]
      }

RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x4563918244f40000"}, 4]
RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x4563918244f40000"}, 4]
  ==> "5"

Bob accepts the wager of 5

RPC /kont ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", null]

RPC /backend/Alice
      [ "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"
      , { "wager": { "type": "BigNumber", "hex": "0x4563918244f40000" }
        , "stdlib.hasRandom": true
        }
      , { "getHand":          true
        , "informTimeout":    true
        , "seeOutcome":       true
        }
      ]
  ==> { "t":    "Kont"
      , "kid":  "1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9"
      , "m":    "getHand"
      , "args": []
      }

Alice played Scissors

RPC /kont ["1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9", 2]

RPC /kont
      ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", null]
  ==> { "t":    "Kont"
      , "kid":  "0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de"
      , "m":    "getHand"
      , "args": []
      }

Bob played Rock

RPC /kont ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", 0]
RPC /kont ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", 0]
  ==> { "t":    "Kont"
      , "kid":  "0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de"
      , "m":    "seeOutcome"
      , "args": [{ "type": "BigNumber", "hex": "0x00" }]
      }

RPC /stdlib/bigNumberToNumber [{"type": "BigNumber", "hex": "0x00"}]

RPC /kont ["1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9", 2]
  ==> { "t":    "Kont"
      , "kid":  "1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9"
      , "m":    "seeOutcome"
      , "args": [{ "type": "BigNumber", "hex": "0x00" }]
      }

RPC /stdlib/bigNumberToNumber [{"type": "BigNumber", "hex": "0x00"}]
RPC /stdlib/bigNumberToNumber [{"type": "BigNumber", "hex": "0x00"}]
  ==> 0

Bob saw outcome Bob wins

RPC /kont ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", null]

RPC /stdlib/bigNumberToNumber [{"type": "BigNumber", "hex": "0x00"}]
  ==> 0

Alice saw outcome Bob wins

RPC /kont ["1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9", null]

RPC /kont
      ["0_75962d0a956ae6583baeb5d36f9d4b995ce9101a5a9ac0de", null]
  ==> {"t": "Done"}

RPC /kont
      ["1_0cfd582d41b4c6b9cc2687620dc6618ad01f0c9a154ad0e9", null]
  ==> {"t": "Done"}

RPC /stdlib/balanceOf
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
RPC /stdlib/balanceOf
      ["0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"]
  ==> {"type": "BigNumber", "hex": "0x4563918244e0d9dc"}

RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x4563918244e0d9dc"}, 4]
RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0x4563918244e0d9dc"}, 4]
  ==> "4.9999"

RPC /stdlib/balanceOf
      ["1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"]
RPC /stdlib/balanceOf
      ["1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"]
  ==> {"type": "BigNumber", "hex": "0xd02ab486cedaf45f"}

RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0xd02ab486cedaf45f"}, 4]
RPC /stdlib/formatCurrency
      [{"type": "BigNumber", "hex": "0xd02ab486cedaf45f"}, 4]
  ==> "14.9999"

Alice went from 10 to 4.9999
  Bob went from 10 to 14.9999

RPC /forget/acc
      [ "0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"
      , "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"
      ]
RPC /forget/acc
      [ "0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"
      , "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"
      ]
  ==> { "deleted":
        [ "0_bd13cd604ce025f3ec001087c2e918a842dcc944161ec3e6"
        , "1_4bccb92055b56c7cba1db3044588dc7d20c4451677c0a973"
        ]
      }

RPC /forget/ctc
      [ "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"
      , "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"
      ]
RPC /forget/ctc
      [ "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"
      , "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"
      ]
  ==> { "deleted":
        [ "0_c1e52c08c9290a009f8525b180d5637316837e549d398d97"
        , "1_c5b17579d5914f9adf551c1da110445fa00fb18a78e97817"
        ]
      }
}

@(hrule)

This tutorial uses Python to demonstrate how RPC @tech{frontends} are
built in Reach, but it is similarly easy to write RPC @tech{frontends} in other
languages, such as with the @seclink{ref-frontends-rpc-js} and
@seclink{ref-frontends-rpc-go} libraries.
