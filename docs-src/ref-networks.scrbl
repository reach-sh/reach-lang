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

@tech{Non-network tokens} are compiled to @link["https://ethereum.org/en/developers/docs/standards/tokens/erc-20/"]{ERC-20} fungible tokens.

@tech{Views} are compiled to @litchar{view} functions.
A @tech{view} named @litchar{X.Y} will be named @litchar{X_Y}.
@tech{Views} expand the on-chain state to include the free variables of all values bound to a @tech{view}.

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
The @|DApp| consists of one application, one contract-controlled escrow account, and many contract-controlled handlers for each of the @tech{publications} of your Reach program.
During compilation, the connector produces intermediate outputs for each of these contracts.
These contracts embed references to each through their template arguments, which is done automatically by the Reach standard library implementation.

It relies on versions of @tt{algod} that support TEAL 3, such as the Algorand BetaNet 2.5.2 release from mid-March 2021.
It uses the Algorand @tt{indexer} version 2 to lookup and monitor @tech{publications}; in other words, it does @emph{not} rely on any communication network other than Algorand itself.

Algorand uses the Keccak256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 64-bits.

@tech{Non-network tokens} are compiled to @link["https://developer.algorand.org/docs/features/asa/"]{Algorand Standard Assets} (ASAs).
Reach programs that use @tech{non-network tokens} deployed on Algorand are inherently vulnerable to a denial-of-service attack due the ability of Algorand accounts to "opt-out" of a token.
For example, if a program has a @tech{consensus step} where Alice will receive 1 gil and Bob will receive 2 zorkmids, either Alice or Bob can prevent this step from executing by opting out of (respectively) gil or zorkmids.
(An "opt-out" is performed by sending an @link["https://developer.algorand.org/docs/reference/transactions/#asset-transfer-transaction"]{Asset Transfer Transaction} (@litchar{axfer}) with a non-zero @litchar{AssetCloseTo} field.)
You can alleviate this problem by ensuring that any @tech{non-network token} transfers occurs as the last consensus steps of the program and may be executed in any order by the recipient of the funds.
We hope that future versions of Algorand will provide a facility for preventing these denial-of-service attacks.

@tech{Views} are compiled to client-side functions that can interpret the global and local state of the Algorand Application associated with the @|DApp|.
This means they are sensitive to the particular compilation details of the particular Reach program.
We hope to work with the Algorand community to define a standard for @tech{views}.
@tech{Views} expand the on-chain state to include the free variables of all values bound to a @tech{view}.

This connector does not support different @reachin{deployMode}s and treats them all as @reachin{'constructor'}.

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

@item{@defenv{ALGO_FAUCET_PASSPHRASE} is used as the mnemonic for the faucet of your network.
This is useful if you are running your own testing network.}

]
