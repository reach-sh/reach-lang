#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-browser-testing"]{Testing Reach programs in the browser}
@author[(author+email "Dan Burton" "dan@reach.sh")]

Reach is designed to work securely in a browser-based Web app
by leveraging browser-based wallets to sign transactions.
This document will give you a few hints about configuring
wallets like MetaMask for Ethereum and AlgoSigner for Algorand.

@;{TODO: MetaMask tips}

@(local-table-of-contents)

@section{Algorand}
The Reach standard library gives you a few options for how to sign transactions for Algorand.
You can use @jsin{reach.setSignStrategy} to configure the behavior of @jsin{reach.getDefaultAccount}.

@subsection{Algorand: mnemonic}

@jsin{
import * as reach from '@"@"reach-sh/stdlib/ALGO';
reach.setSignStrategy('mnemonic');
}

The mnemonic strategy is the simplest, and is the default.
Reach prompts the user for their mnemonic, and uses it to derive the secret key needed for signing.

@subsection{Algorand: AlgoSigner}

@jsin{
import * as reach from '@"@"reach-sh/stdlib/ALGO';
reach.setSignStrategy('AlgoSigner');
}

The AlgoSigner strategy will prompt the user for their account address,
and will use AlgoSigner to sign transactions.

When Reach programs are run on a development network, like the one created by @exec{reach run}, AlgoSigner must be configured appropriately.
Here's how to configure AlgoSigner for the Reach Devnet:

@itemlist[
 @item{@link["https://chrome.google.com/webstore/detail/algosigner/kmmolakhbgdlpkjkcjkebenjheonagdm"]{Install the extension}}
 @item{Click the extension}
 @item{Click the gear icon on the top-right of the extension popover}
 @item{Click "Network Configuration"}
 @item{Click "New Network"}
 @item{Fill in the config below and click "Save"}
 @item{Click the dropdown and select "Reach Devnet"}
 @item{Click "Add account" to add accounts for use in local testing}
]

The configuration:

@itemlist[
 @item{Display Name: Reach Devnet}
 @item{Network ID: devnet-v1}
 @item{Network ALgod URL: http://localhost:4180}
 @item{Network Indexer URL: http://localhost:8980}
 @item{Network Headers: @jsin{{"Algod": {"X-Algo-API-Token": "c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705"}, "Indexer": {"X-Indexer-API-Token": "reach-devnet"}}}}
]

@;{TODO: screenshots}
