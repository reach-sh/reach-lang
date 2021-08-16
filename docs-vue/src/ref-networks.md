


# {#ref-networks} Consensus Network Connectors

This section describes the consensus network connectors
supported by Reach version {{ VERSION }}.

[[toc]]

## {#ref-network-eth} Ethereum

The [Ethereum](https://ethereum.org/) Reach connector generates a contract that
manages one instance of the DApp's execution.
It is guaranteed to
use exactly one word of on-chain state, while each piece of consensus state appears as a transaction argument.

Ethereum uses the Keccak256 algorithm to perform digests.
Its bit width is 256-bits.

Non-network tokens are compiled to [ERC-20](https://ethereum.org/en/developers/docs/standards/tokens/erc-20/) fungible tokens.
Specifically, the `Token` type refers to the address of the ERC-20 contract.
Token minting launches a fresh ERC-20 contract based on the OpenZeppelin ERC-20 implementation, which stores additional metadata and allows the creator to burn tokens and destroy the token if there is no supply (i.e. it has all been burned).

Views are compiled to `view` functions.
A view named `X.Y` will be named `X_Y`.
Views expand the on-chain state to include the free variables of all values bound to a view.

In Ethereum, network time corresponds to block numbers and network seconds correspond to the Unix timestamp of the block.

The connector provides a binding named `ETH` to
backends.

During compilation, the connector produces one intermediate output: `input.export.sol`, containing
the Solidity code implementing the contract.

A few details of Ethereum leak through to Reach.
The node that a given participant is connected to does not instantly know that its blocks are correct and may revert past transactions after it reaches consensus with the rest of the network.
This means that Reach applications must not make externally observable effects until after such consensus is reached.

Backends must respect the following environment variables:

+ `ETH_NODE_URI` is used to contact the Ethereum node.
It defaults to `http://localhost:8545`.
+ `ETH_NODE_NETWORK` is used to name the Ethereum network.
It defaults to `unspecified`.


## {#ref-network-algo} Algorand

The [Algorand](https://www.algorand.com/) Reach connector generates a set of
contracts that manage one instance of the DApp's
execution.

It uses finite on-chain state.
The DApp consists of one application and one contract-controlled escrow account.

It relies on versions of `algod` that support TEAL version 4, such as Algorand 2.7.1 from July 2021.
It uses the Algorand `indexer` version 2 to lookup and monitor publications; in other words, it does _not_ rely on any communication network other than Algorand itself.

Algorand uses the SHA256 algorithm to perform digests.
Its bit width is 64-bits.

Non-network tokens are compiled to [Algorand Standard Assets](https://developer.algorand.org/docs/features/asa/) (ASAs).
Specifically, the `Token` type refers to the id of the ASA.
Reach programs that use non-network tokens deployed on Algorand are inherently vulnerable to a denial-of-service attack due to the ability of Algorand accounts to "opt-out" of a token.
For example, if a program has a consensus step where Alice will receive 1 gil and Bob will receive 2 zorkmids, either Alice or Bob can prevent this step from executing by opting out of (respectively) gil or zorkmids.
(An "opt-out" is performed by sending an [Asset Transfer Transaction](https://developer.algorand.org/docs/reference/transactions/#asset-transfer-transaction) (`axfer`) with a non-zero `AssetCloseTo` field.)
You can alleviate this problem by ensuring that any non-network token transfers occur as the last consensus steps of the program and may be executed in any order by the recipient of the funds.
We hope that future versions of Algorand will provide a facility for preventing these denial-of-service attacks.

Token minting creates an ASA owned and managed by the contract account.
Freezing, clawback, reserves, and separate managers are not supported.

Views are compiled to client-side functions that can interpret the global and local state of the Algorand Application associated with the DApp.
This means they are sensitive to the particular compilation details of the particular Reach program.
We hope to work with the Algorand community to define a standard for views.
Views expand the on-chain state to include the free variables of all values bound to a view.

Linear state is compiled into Application Local State.
This means that participants must explicitly "opt-in" to storing this state on their account (which increases their minimum balance).
The Reach standard library will do this automatically when connecting to Reach generated contracts, but other users must be specifically programmed to do this.
This "opt-in" requirement means that DApps with linear state deployed on Algorand can deadlock and be held hostage:
Suppose that Alice transfers 10 ALGO to a contract in step one, then in step two, the consensus must store a value associated with Bob, and then she can receive her 10 ALGO back, then the program terminates.
On some networks, Alice can perform these two steps completely on her own and she is in complete control of her funds.
However, on Algorand, running this program requires that Bob "opt-in" to storing values for the application.
We hope that future versions of Algorand will allow other parties to pay the fees to "opt-in" to applications to prevent these kinds of deadlock attacks.

In Algorand, network time corresponds to round numbers and network seconds correspond to the Unix timestamp of the previous round.
(This is because the current round's timestamp is not determined until after it is finalized.
This means that a network second-based deadline could be exceeded by the round time of the network, which is typically five seconds.)

This connector does not support different `deployMode`s and treats them all as `'constructor'`.

The connector provides a binding named `ALGO` to
backends.

Backends must respect the following environment variables:

+ `ALGO_TOKEN` is used as the API token for your `algod`.
+ `ALGO_SERVER` is used as the address of your `algod`.
+ `ALGO_PORT` is used as the port of your `algod`.
+ `ALGO_INDEXER_TOKEN` is used as the API token for your `indexer`.
+ `ALGO_INDEXER_SERVER` is used as the address of your `indexer`.
+ `ALGO_INDEXER_PORT` is used as the port of your `indexer`.
+ `ALGO_FAUCET_PASSPHRASE` is used as the mnemonic for the faucet of your network.
This is useful if you are running your own testing network.

