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

Ethereum uses the Keccak256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 256-bits.

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

@item{@defenv{ETH_NODE_URI} is used to contact the Ethereum node.
It defaults to @litchar{http://localhost:8545}.}

@item{@defenv{ETH_NODE_NETWORK} is used to name the Ethereum network.
It defaults to @litchar{unspecified}.}

]

@section[#:tag "ref-network-algo"]{Algorand}

The @link["https://www.algorand.com/"]{Algorand} Reach @tech{connector} generates a set of
@tech{contracts} that manage one instance of the @|DApp|'s
execution.

It uses finite on-chain state: two integers and one byte string.
The DApp consists of one application, one contract-controlled escrow account, and many contract-controlled handlers for each of the @tech{publications} of your Reach program.
During compilation, the connector produces intermediate outputs for each of these contracts.
These contracts embed references to each through their template arguments, which is done automatically by the Reach standard library implementation.

It relies on a patched version of @tt{algod} that includes @link["https://github.com/algorand/go-algorand/pull/1533"]{our implementation of stateless contract argument inspection}.
It uses the Algorand @tt{indexer} version 2 to lookup and monitor @tech{publications}; in other words, it does @emph{not} rely on any communication network other than Algorand itself.

Algorand uses the Keccak256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 64-bits.

This connector does not support different @reachin{deployMode}s and treats them all as @reachin{constructor}.

The connector provides a binding named @reachin{ALGO} to
@tech{backends}.

@tech{Backends} must respect the following environment variables:

@itemlist[

@item{@defenv{ALGO_TOKEN} is used as the API token for your @tt{algod}.}
@item{@defenv{ALGO_SERVER} is used as the address of your @tt{algod}.}
@item{@defenv{ALGO_PORT} is used as the port of your @tt{algod}.}

@item{@defenv{ALGO_INDEXER_TOKEN} is used as the API token for your @tt{indexer}.}
@item{@defenv{ALGO_INDEXER_SERVER} is used as the address of your @tt{indexer}.}
@item{@defenv{ALGO_INDEXER_PORT} is used as the port of your @tt{indexer}.}

]
