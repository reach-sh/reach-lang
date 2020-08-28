#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-networks" #:style
'toc]{Consensus Network Connectors}

This section describes the @tech{consensus network} @tech{connectors}
supported by Reach version @|reach-vers|.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "ref-network-eth"]{Ethereum}

The @link["https://ethereum.org/"]{Ethereum} Reach @tech{connector} generates a @tech{contract} that
manages one instance of the @|DApp|'s execution. It is guaranteed to
use exactly one word of on-chain state, while each piece of @tech{consensus state} appears as a transaction argument.

The connector provides a binding named @reachin{ETH} to
@tech{backends}.

During compilation, the connector produces one intermediate output: @filepath{input.export.sol}, containing
the Solidity code implementing the @tech{contract}.

A few details of Ethereum leak through to Reach.
In Ethereum, @tech{time} corresponds to block numbers.
The node that a given @tech{participant} is connected to does not instantly know that it's blocks are correctly and may revert past transactions after it reaches consensus with the rest of the network.
This means that Reach applications must not make externally observable effects until after such consensus is reached.

@tech{Backends} must respect the following environment variables:

@itemlist[

@item{@envvar{ETH_NODE_TYPE} may be either @litchar{in_memory_ganache}, to use an in memory instance of Ganache, or @litchar{uri} to use a URI.}

@item{@envvar{ETH_NODE_URI} is used to contact the Ethereum node.
It defaults to @litchar{http://localhost:8545}.}

@item{@envvar{ETH_NODE_NETWORK} is used to name the Ethereum network.
It defaults to @litchar{unspecified}.}

]

@section[#:tag "ref-network-algo"]{Algorand} @(experimental)

The @link["https://www.algorand.com/"]{Algorand} Reach @tech{connector} generates a set of
@tech{contracts} that manage one instance of the @|DApp|'s
execution. It relies on the "Application" feature of the Algorand
network. It uses two words of on-chain state in two application
keys.

The connector provides a binding named @reachin{ALGO} to
@tech{backends}.

During compilation, the connector produces two intermediate outputs
corresponding to each of these fields: @filepath{input.export.app.teal}, containing
the TEAL code implementing the @tech{contract} as an
"Application"-mode @tech{contract}, as well as
@filepath{input.export.lsp.teal}, containing the TEAL code implementing a
portion of the @tech{contract} as a logic signature program.

It is not guaranteed to produce contracts that obey the size or cost
limits of the Algorand network, nor does it produce any warning when
it violates these limits.
