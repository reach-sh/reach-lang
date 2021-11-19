---
menuItem: mi-docs
---

# Algorand

The [Algorand](https://www.algorand.com/) Reach connector generates a set of contracts that manage one instance of the DApp's execution.

It uses finite on-chain state. The DApp consists of one application and one contract-controlled escrow account.

It relies on versions of `algod` that support TEAL version 4, such as Algorand 2.7.1 from July 2021. It uses the Algorand `indexer` version 2 to lookup and monitor publications; in other words, it does not rely on any communication network other than Algorand itself.

Algorand uses the SHA256 algorithm to perform digests. Its bit width is 64-bits.

Non-network tokens are compiled to Algorand Standard Assets (ASAs). Specifically, the `Token` type refers to the id of the ASA.

Token minting creates an ASA owned and managed by the contract account. Freezing, clawback, reserves, and separate managers are not supported.

Views are compiled to client-side functions that can interpret the global and local state of the Algorand Application associated with the DApp. This means they are sensitive to the particular compilation details of the particular Reach program. We hope to work with the Algorand community to define a standard for views. Views expand the on-chain state to include the free variables of all values bound to a view.

Linear state is compiled into Application Local State. This means that participants must explicitly "opt-in" to storing this state on their account (which increases their minimum balance). The Reach standard library will do this automatically when connecting to Reach generated contracts, but other users must be specifically programmed to do this. This "opt-in" requirement means that DApps with linear state deployed on Algorand can deadlock and be held hostage: Suppose that Alice transfers 10 ALGO to a contract in step one, then in step two, the consensus must store a value associated with Bob, and then she can receive her 10 ALGO back, then the program terminates. On some networks, Alice can perform these two steps completely on her own and she is in complete control of her funds. However, on Algorand, running this program requires that Bob "opt-in" to storing values for the application. We hope that future versions of Algorand will allow other parties to pay the fees to "opt-in" to applications to prevent these kinds of deadlock attacks.

In Algorand, network time corresponds to round numbers and network seconds correspond to the Unix timestamp of the previous round. (This is because the current round's timestamp is not determined until after it is finalized. This means that a network second-based deadline could be exceeded by the round time of the network, which is typically five seconds.)

The connector provides a binding named `ALGO` to backends.

Backends must respect the following environment variables:

* `ALGO_TOKEN` is used as the API token for your algod.
* `ALGO_SERVER` is used as the address of your algod.
* `ALGO_PORT` is used as the port of your algod.
* `ALGO_INDEXER_TOKEN` is used as the API token for your indexer.
* `ALGO_INDEXER_SERVER` is used as the address of your indexer.
* `ALGO_INDEXER_PORT` is used as the port of your indexer.
* `ALGO_FAUCET_PASSPHRASE` is used as the mnemonic for the faucet of your network. This is useful if you are running your own testing network.