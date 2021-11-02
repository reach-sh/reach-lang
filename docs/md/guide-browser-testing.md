


# {#guide-browser-testing} Testing Reach programs in the browser


Reach is designed to work securely in a browser-based Web app
by leveraging browser-based wallets to sign transactions.
This document will give you a few hints about configuring
wallets like MetaMask for Ethereum and AlgoSigner for Algorand.



[[toc]]

## Algorand
The Reach standard library gives you a few options for how to sign transactions for Algorand.
You can use `reach.setSignStrategy` to configure the behavior of `reach.getDefaultAccount`.

### Algorand: mnemonic

`import * as reach from '@reach-sh/stdlib/ALGO';
reach.setSignStrategy('mnemonic');`

The mnemonic strategy is the simplest, and is the default.
Reach prompts the user for their mnemonic, and uses it to derive the secret key needed for signing.

### Algorand: AlgoSigner

`import * as reach from '@reach-sh/stdlib/ALGO';
reach.setSignStrategy('AlgoSigner');`

The AlgoSigner strategy will prompt the user for their account address,
and will use AlgoSigner to sign transactions.

When Reach programs are run on a development network, like the one created by `reach run`, AlgoSigner must be configured appropriately.
Here's how to configure AlgoSigner for the Reach Devnet:

+ [Install the extension](https://chrome.google.com/webstore/detail/algosigner/kmmolakhbgdlpkjkcjkebenjheonagdm)
+ Click the extension
+ Click the gear icon on the top-right of the extension popover
+ Click "Network Configuration"
+ Click "New Network"
+ Fill in the config below and click "Save"
+ Click the dropdown and select "Reach Devnet"
+ Click "Add account" to add accounts for use in local testing


The configuration:

+ Display Name: Reach Devnet
+ Network ID: devnet-v1
+ Network Algod URL: http://localhost:4180
+ Network Indexer URL: http://localhost:8980
+ Network Headers: `{"Algod": {"X-Algo-API-Token": "c87f5580d7a866317b4bfe9e8b8d1dda955636ccebfa88c12b414db208dd9705"}, "Indexer": {"X-Indexer-API-Token": "reach-devnet"}}`



