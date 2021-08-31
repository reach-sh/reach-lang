#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-networks" #:style
'toc]{Consensus Network Connectors}

This section describes the @tech{consensus network} @tech{connectors}
supported by Reach version @|reach-vers|.

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "ref-network-eth"]{Ethereum}

The @link["https://ethereum.org/"]{Ethereum} Reach @tech{connector} generates a @tech{contract} that
manages one instance of the @|DApp|'s execution.
It is guaranteed to
use exactly one word of on-chain state, while each piece of @tech{consensus state} appears as a transaction argument.

Ethereum uses the Keccak256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 256-bits.

@tech{Non-network tokens} are compiled to @link["https://ethereum.org/en/developers/docs/standards/tokens/erc-20/"]{ERC-20} fungible tokens.
Specifically, the @reachin{Token} type refers to the address of the ERC-20 contract.
@tech{Token minting} launches a fresh ERC-20 contract based on the OpenZeppelin ERC-20 implementation, which stores additional metadata and allows the creator to burn tokens and destroy the token if there is no supply (i.e. it has all been burned).

@tech{Views} are compiled to @litchar{view} functions.
A @tech{view} named @litchar{X.Y} will be named @litchar{X_Y}.
@tech{Views} expand the on-chain state to include the free variables of all values bound to a @tech{view}.

In Ethereum, @tech{network time} corresponds to block numbers and @tech{network seconds} correspond to the Unix timestamp of the block.

The connector provides a binding named @reachin{ETH} to
@tech{backends}.

During compilation, the connector produces one intermediate output: @filepath{input.export.sol}, containing
the Solidity code implementing the @tech{contract}.

A few details of Ethereum leak through to Reach.
The node that a given @tech{participant} is connected to does not instantly know that its blocks are correct and may revert past transactions after it reaches consensus with the rest of the network.
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

It uses finite on-chain state.
The @|DApp| consists of one application and one contract-controlled escrow account.

It relies on versions of @tt{algod} that support TEAL version 4, such as Algorand 2.7.1 from July 2021.
It uses the Algorand @tt{indexer} version 2 to lookup and monitor @tech{publications}; in other words, it does @emph{not} rely on any communication network other than Algorand itself.

Algorand uses the SHA256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 64-bits.

@tech{Non-network tokens} are compiled to @link["https://developer.algorand.org/docs/features/asa/"]{Algorand Standard Assets} (ASAs).
Specifically, the @reachin{Token} type refers to the id of the ASA.

@tech{Token minting} creates an ASA owned and managed by the contract account.
Freezing, clawback, reserves, and separate managers are not supported.

Algorand's ASAs have some inherent design flaws that inhibit reasoning about them.
First, the "freezing" "feature" disables the ability of contracts (and users) from transfering their assets, without prior notification that the asset is frozen (i.e., in one moment, a contract may transfer an asset, but then in the next moment, without any change to the contract state, it ceases to be able to transfer it, because elsewhere in the network, an asset was frozen.)
Second, the "clawback" "feature" extracts assets from an account without approval by or notification of the account holder.
Third, the "opt-in" "feature" prevents accounts from transfering assets without prior approval by the receiver.
Fourth, the "opt-out" "feature" allows accounts to take-back the permission to transfer assets to them after having previously given it.
Each of these issues mean that rely-guarantee reasoning is not appropriate for analyzing consensus steps on Algorand: in other words, it is not possible to guarantee from the structure of a program that consensus steps which are dominated by honest interactions will succeed, because external agents can arbitrarily change the semantics of consensus operations.
This essentially amounts to all Algorand DApps that use @tech{non-network tokens} being inherently vulnerable to denial-of-service attacks when these "features" are used.
For example, if a DApp has a @tech{consensus step} where Alice will receive 1 gil and Bob will receive 2 zorkmids, either Alice or Bob can prevent this step from executing by opting out of (respectively) gil or zorkmids.
(An "opt-out" is performed by sending an @link["https://developer.algorand.org/docs/reference/transactions/#asset-transfer-transaction"]{Asset Transfer Transaction} (@litchar{axfer}) with a non-zero @litchar{AssetCloseTo} field.)
You can alleviate this problem by ensuring that any @tech{non-network token} transfers occur as the last consensus steps of the program and may be executed in any order by the recipient of the funds.
Similarly, if a DApp accepts a @tech{non-network token} that has enabled clawback, then it can be prevented from progressing if the manager takes the contract's tokens behind its back.
Unfortunately, there is no way to alleviate this, other than refusing to accept tokens that have the possibility of clawback, but on Algorand, tokens default to allowing clawback, so that is not feasible.
Rather than simplying rejecting all programs that use @tech{non-network tokens} on Algorand, Reach allows them, with these caveats about the reliability of the generated contracts.
We hope that future versions of Algorand will provide a facility for preventing these attacks, such as by removing these "features".

@tech{Views} are compiled to client-side functions that can interpret the global and local state of the Algorand Application associated with the @|DApp|.
This means they are sensitive to the particular compilation details of the particular Reach program.
We hope to work with the Algorand community to define a standard for @tech{views}.
@tech{Views} expand the on-chain state to include the free variables of all values bound to a @tech{view}.

@tech{Linear state} is compiled into Application Local State.
This means that participants must explicitly "opt-in" to storing this state on their account (which increases their minimum balance).
The Reach standard library will do this automatically when connecting to Reach generated contracts, but other users must be specifically programmed to do this.
This "opt-in" requirement means that @|DApps| with @tech{linear state} deployed on Algorand can deadlock and be held hostage:
Suppose that Alice transfers 10 ALGO to a contract in step one, then in step two, the consensus must store a value associated with Bob, and then she can receive her 10 ALGO back, then the program terminates.
On some networks, Alice can perform these two steps completely on her own and she is in complete control of her funds.
However, on Algorand, running this program requires that Bob "opt-in" to storing values for the application.
We hope that future versions of Algorand will allow other parties to pay the fees to "opt-in" to applications to prevent these kinds of deadlock attacks.

In Algorand, @tech{network time} corresponds to round numbers and @tech{network seconds} correspond to the Unix timestamp of the previous round.
(This is because the current round's timestamp is not determined until after it is finalized.
This means that a @tech{network second}-based deadline could be exceeded by the round time of the network, which is typically five seconds.)

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
