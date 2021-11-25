#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "ref-networks" #:style
'toc]{Consensus Network Connectors}

This section describes the @tech{consensus network} @tech{connectors}
supported by Reach version @|reach-vers|:
@itemlist[
@item{@secref["ref-network-algo"]}
@item{@secref["ref-network-eth"]}
@item{@secref["ref-network-cfx"]}
]

@section[#:tag "ref-network-algo"]{Algorand}

The @link["https://www.algorand.com/"]{Algorand} Reach @tech{connector} generates a
@tech{contract} that manage one instance of the @|DApp|'s
execution.

It uses finite on-chain state.
The @|DApp| consists of one application.
The contract escrow account is the application account.

It relies on versions of @tt{algod} that support TEAL version 5, such as Algorand 3.0.1 from September 2021.
It uses the Algorand @tt{indexer} version 2 to lookup and monitor @tech{publications}; in other words, it does @emph{not} rely on any communication network other than Algorand itself.

Algorand uses the SHA256 algorithm to perform @tech{digest}s.
Its @tech{bit width} is 64-bits.

@tech{Non-network tokens} are compiled to @link["https://developer.algorand.org/docs/features/asa/"]{Algorand Standard Assets} (ASAs).
Specifically, the @reachin{Token} type refers to the id of the ASA.

@tech{Token minting} creates an ASA owned and managed by the contract account.
Freezing, clawback, reserves, and separate managers are not supported.

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

@section[#:tag "ref-network-cfx"]{Conflux}

The @link["https://confluxnetwork.org/"]{Conflux} Reach @tech{connector} works almost identically to the @seclink["ref-network-eth"]{Ethereum connector}, except that it behaves differently at runtime: using, for example, @link["https://portal.confluxnetwork.org/"]{Conflux Portal} rather than @link["https://metamask.io/"]{MetaMask}, and connecting to Conflux nodes.

@tech{Backends} must respect the following environment variables:

@itemlist[

@item{@defenv{CFX_NODE_URI} is used to contact the Conflux node.
It defaults to @litchar{http://localhost:12537}.}

@item{@defenv{CFX_NETWORK_ID} is used to determine the Conflux network id.
It defaults to @litchar{999}.}

]

@subsection[#:tag "cfx-faq"]{FAQ}

@subsubsection[#:tag "cfx-faq-mainnet"]{How do I run my Reach DApp on CFX TestNet or MainNet?}

You can add the following JavaScript near the beginning of your index.js or index.mjs file
in order to run on Conflux TestNet:

@js{
reach.setProviderByName('TestNet');
}

Or this to run on Conflux MainNet:

@js{
reach.setProviderByName('MainNet');
}

@subsubsection[#:tag "cfx-faq-cplocal"]{How can I use ConfluxPortal with the Reach devnet?}

If you find that ConfluxPortal's Localhost 12537 default configuration does not work correctly with Reach apps,
you can try configuring ConfluxPortal to use a custom RPC endpoint:

@itemlist[
  @item{Click the network dropdown in Conflux Portal}
  @item{Select: Custom RPC}
  @item{Use RPC url: http://127.0.0.1:12537}
]

If your locally-running Conflux devnet restarts,
you may find that you need to reset ConfluxPortal's account history,
which you can do like so:

@itemlist[
  @item{Select the desired account}
  @item{Click the profile image of the account (top-right)}
  @item{Click Settings > Advanced > Reset Account > (confirm) Reset}
  @item{Switch to a different network and back}
  @item{CTRL+SHIFT+R to hard-reset the webpage.}
]

@subsubsection[#:tag "cfx-faq-sponsor"]{How can I use gas and storage sponsorship?}

Conflux, like many other networks, charges fees for using the network and interacting with smart contracts.
These fees are normally pay by the originator the transaction: who we would call the "publisher" in Reach.
However, unlike other networks, Conflux does not require these fees to be paid for by the originator.
Instead, Conflux allows fees to be paid for by a third-party.
This is called "sponsorship".

Conflux maintains documentation for this feature on their @link["https://developer.confluxnetwork.org/conflux-rust/internal_contract/internal_contract/"]{internal contract} documentation page.
You can follow the directions on the Conflux page exactly and it will work with Reach, because Reach programs, when deployed on Conflux, are just normal programs.

We duplicate these instructions below; if you have any difficulties with these directions, please refer to the @link["https://developer.confluxnetwork.org/conflux-rust/internal_contract/internal_contract/"]{Conflux source} and then let us know, so we can update them.

@(hrule)

There are three steps to enabling sponsorship.

First, you have to directly interact with @link["https://github.com/Conflux-Chain/js-conflux-sdk"]{the Conflux SDK}.
This will require adding an @tt{npm} package dependency and initializing the library.
The @link["https://confluxnetwork.gitbook.io/js-conflux-sdk/quick_start"]{Conflux SDK Quickstart} walks through this process.
It will look something like:
@js{
// import Conflux Class
const { Conflux } = require('js-conflux-sdk');
// initialize a Conflux object
const conflux = new Conflux({
  // some options
});
}

Second, you must call the @litchar{addPrivilegeByAdmin} Conflux internal contract from the creator (i.e. deployer) of the smart contract.
This function requires you to provide an address that is eligible for sponsorship.
It is documented in @link["https://developer.confluxnetwork.org/conflux-rust/internal_contract/internal_contract/#whitelist-maintenance"]{this section} of the Conflux manual.
If you use an address which is all zeros, then every address is sponsored.
The code will look something like:
@js{
const sponsor_contract = conflux.InternalContract('SponsorWhitelistControl');
await sponsor_contract.addPrivilegeByAdmin(contractAddress, ["0x0000000000000000000000000000000000000000"])
  .sendTransaction({from: creatorAddress});
}
In this code sample, we assume that @jsin{contractAddress} is the address of the Reach contract, which you might have acquired via @jsin{await ctc.getContractAddress()} and @jsin{creatorAddress} is the address of the creator of the Reach contract.

Third, you have to provide a sponsoring account by calling @litchar{setSponsorForGas} (or @litchar{setSponsorForCollateral}) on the Conflux internal contract.
This function must be called by the sponsoring account.
It is documented in @link["https://developer.confluxnetwork.org/conflux-rust/internal_contract/internal_contract/#add-sponsor-balance"]{this section} of the Conflux manual.
The code will look something like:
@js{
await sponsor_contract.setSponsorForGas(contractAddress, amount)
  .sendTransaction({from: sponsorAddress});
}
In this code sample, we assume that @jsin{contractAddress} is the address of the Reach contract, which you might have acquired via @jsin{await ctc.getContractAddress()}, @jsin{amount} is the maximum amount the sponsor is willing to sponsor, and @jsin{sponsorAddress} is the address of the sponsor.

@(hrule)

If your application is made of equal peers, you may not want to enable the sponsorship feature.
But, if your application has a clear party with extra authority and resources, you might like to make them the creator and sponsor of the contract, because this incentivizes participation in the program by lowering the cost to do so.

As of November 2021, the Conflux Foundation is willing to sponsor some smart contracts.
@link["https://forum.conflux.fun/t/announcement-usage-adjustment-of-ecosystem-fund-for-the-conflux-sponsorship-mechanism-2022-11-11/12056"]{This forum post} discusses how to request sponsorship of your contract.
If the Foundation agrees to sponsor your program, then you only need to do steps one and two above: you can skip step three.

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
A @tech{view} named @litchar{X} will be named @litchar{X}.
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
