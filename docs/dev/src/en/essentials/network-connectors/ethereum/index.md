---
menuItem: mi-docs
---

# Ethereum

The [Ethereum](https://ethereum.org/) Reach connector generates a contract that manages one instance of the DApp's execution. It is guaranteed to use exactly one word of on-chain state, while each piece of consensus state appears as a transaction argument.

Ethereum uses the Keccak256 algorithm to perform digests. Its bit width is 256-bits.

Non-network tokens are compiled to [ERC-20](https://ethereum.org/en/developers/docs/standards/tokens/erc-20/) fungible tokens. Specifically, the `Token` type refers to the address of the ERC-20 contract. Token minting launches a fresh ERC-20 contract based on the OpenZeppelin ERC-20 implementation, which stores additional metadata and allows the creator to burn tokens and destroy the token if there is no supply (i.e. it has all been burned).

Views are compiled to view functions. A view named `X.Y` will be named `X_Y`. A view named X will be named `X`. Views expand the on-chain state to include the free variables of all values bound to a view.

In Ethereum, network time corresponds to block numbers and network seconds correspond to the Unix timestamp of the block.

The connector provides a binding named `ETH` to backends.

During compilation, the connector produces one intermediate output: `input.export.sol`, containing the Solidity code implementing the contract.

A few details of Ethereum leak through to Reach. The node that a given participant is connected to does not instantly know that its blocks are correct and may revert past transactions after it reaches consensus with the rest of the network. This means that Reach applications must not make externally observable effects until after such consensus is reached.

Backends must respect the following environment variables:

`ETH_NODE_URI` is used to contact the Ethereum node. It defaults to `http://localhost:8545`.
`ETH_NODE_NETWORK` is used to name the Ethereum network. It defaults to `unspecified`.
