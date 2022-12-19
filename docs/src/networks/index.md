# {#ref-networks} Networks

This section describes the consensus network connectors
supported by Reach version @{VERSION}:
+ @{seclink("ref-network-algo")}
+ @{seclink("ref-network-eth")}

## {#ref-network-algo} Algorand

The [Algorand](https://www.algorand.com/) Reach connector generates a
contract that manage one instance of the DApp's
execution.

It uses finite on-chain state.
The DApp consists of one application.
The contract escrow account is the application account.
Sometimes a companion contract is created that is used to increase the opcode budget during contract execution.

It relies on versions of `algod` that support TEAL version 6, such as Algorand 3.5.1.
It uses the Algorand `indexer` version 2 to lookup and monitor publications; in other words, it does _not_ rely on any communication network other than Algorand itself.

Algorand uses the SHA256 algorithm to perform digests.
Its bit width is 64-bits.

Non-network tokens are compiled to [Algorand Standard Assets](https://developer.algorand.org/docs/features/asa/) (ASAs).
Specifically, the `{!rsh} Token` type refers to the id of the ASA.

Token minting creates an ASA owned and managed by the contract account.
Freezing, clawback, reserves, and separate managers are not supported.

Views are compiled to client-side functions that can interpret the global and local state of the Algorand Application associated with the DApp.
This means they are sensitive to the particular compilation details of the particular Reach program.
We hope to work with the Algorand community to define a standard for views.
Views expand the on-chain state to include the free variables of all values bound to a view.

Linear state is compiled into application box storage.
The names of boxes are either:
- `${MapIndexByte}${KeyBytes}`, where `MapIndexByte` is the single-byte representation of which `{!rsh} Map` it is and `KeyBytes` is the ABI encoding of the key, if this is less than 64 bytes; or
- `sha256(${MapIndexBytes}${KeyBytes})`, where `MapIndexBytes` is the multi-byte representation of the `{!rsh} Map` it is, otherwise.

Reach makes no attempt to ensure that when the application ends, all boxes are freed.
You should read about the `{!rsh} ALGOExitMode` option for `{!rsh} setOptions` for more information about the issues this poses.

In Algorand, network time corresponds to round numbers.
Each round is assigned a Unix timestamp, but when you look at the timestamp in code executing in round N+1, you read the timestamp assigned to round N.
Ensure that you read @{seclink("RW0006")} if you use network seconds.

The connector provides a binding named `{!rsh} ALGO` to backends.

Reach uses the following environment variables:

+ `ALGO_TOKEN` is used as the API token for your `algod`.
  When left unspecified, this defaults to the token of the Reach devnet.
+ `ALGO_TOKEN_HEADER` is used as the HTTP header to share the `algod` API token.
  When left unspecified, this defaults to `X-Algo-API-Token`.
+ `ALGO_SERVER` is used as the address of your `algod`.
  When left unspecified, this defaults to `http://localhost`.
+ `ALGO_PORT` is used as the port of your `algod`.
  When left unspecified, this defaults to `4180`.
+ `ALGO_INDEXER_TOKEN` is used as the API token for your `indexer`.
  When left unspecified, this defaults to the token of the Reach devnet.
+ `ALGO_INDEXER_TOKEN_HEADER` is used as the HTTP header to share the `indexer` API token.
  When left unspecified, this defaults to `X-Indexer-API-Token`.
+ `ALGO_INDEXER_SERVER` is used as the address of your `indexer`.
  When left unspecified, this defaults to `http://localhost`.
+ `ALGO_INDEXER_PORT` is used as the port of your `indexer`.
  When left unspecified, this defaults to `8980`.
+ `ALGO_NODE_WRITE_ONLY` specifies if your `algod` will handle read requests,
  or just writes.
  When left unspecified, this defaults to `no`.
+ `ALGO_FAUCET_PASSPHRASE` is used as the mnemonic for the faucet of your network.
  When left unspecified, this defaults to the passphrase of the Reach devnet's faucet.
  This is useful if you are running your own testing network.
+ `ALGO_GENESIS_ID` or `ALGO_GENESIS_HASH` (or both),
  which allows you to request that the user's [ARC-6 compliant wallet](https://github.com/algorandfoundation/ARCs/blob/main/ARCs/arc-0006.md) connect to a specific network.
  When left unspecified, it allows the user to select one of their wallet's supported networks.
+ `ALGO_ACCOUNT`,
  which allows you to request the use of a specific account from the user's ARC-6 compliant wallet by address.
  This should usually be left unspecified, which allows the user to instead select their preferred account.

## {#ref-network-eth} Ethereum

The [Ethereum](https://ethereum.org/) Reach connector generates a contract that
manages one instance of the DApp's execution.
It is guaranteed to
use exactly one word of on-chain state, while each piece of consensus state appears as a transaction argument.

Ethereum uses the Keccak256 algorithm to perform digests.
Its bit width is 256-bits.

Non-network tokens are compiled to [ERC-20](https://ethereum.org/en/developers/docs/standards/tokens/erc-20/) fungible tokens.
Specifically, the `{!rsh} Token` type refers to the address of the ERC-20 contract.
Token minting launches a fresh ERC-20 contract based on the OpenZeppelin ERC-20 implementation, which stores additional metadata and allows the creator to burn tokens and destroy the token if there is no supply (i.e. it has all been burned).

Views are compiled to `view` functions.
A view named `X.Y` will be named `X_Y`.
A view named `X` will be named `X`.
Views expand the on-chain state to include the free variables of all values bound to a view.

In Ethereum, network time corresponds to block numbers and network seconds correspond to the Unix timestamp of the block.
Ensure that you read @{seclink("RW0006")} if you use network seconds.

The connector provides a binding named `{!rsh} ETH` to
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
