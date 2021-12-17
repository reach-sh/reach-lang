---
menuItem: mi-docs
---

# Conflux

The [Conflux](https://confluxnetwork.org/) Reach connector works almost identically to the [Ethereum](/en/essentials/network-connectors/ethereum/) connector, except that it behaves differently at runtime: using, for example, Conflux Portal rather than MetaMask, and connecting to Conflux nodes.

Backends must respect the following environment variables:

`CFX_NODE_URI` is used to contact the Conflux node. It defaults to `http://localhost:12537`.
`CFX_NETWORK_ID` is used to determine the Conflux network id. It defaults to `999`.

FAQ
How do I run my Reach DApp on CFX TestNet or MainNet?
You can add the following JavaScript near the beginning of your index.js or index.mjs file in order to run on Conflux TestNet:

``` js nonum
reach.setProviderByName('TestNet');
```

Or this to run on Conflux MainNet:

``` js nonum
reach.setProviderByName('MainNet');
```

It is strongly recommended that you also use `setQueryLowerBound` to avoid waiting for unnecessary queries. For example, this code snippet sets the lower bound at 2000 blocks ago:

``` js nonum
const now = await reach.getNetworkTime();
reach.setQueryLowerBound(reach.sub(now, 2000));
```

Why is DApp startup very slow? Why do I need to use `setQueryLowerBound`?

DApp startup doesn't have to be slow. Reach relies on querying Conflux event logs in order to run the DApp. The Conflux network does not yet provide fast APIs for querying event logs for a given contract across all time, so instead, Reach incrementally queries across chunks of 1000 blocks at a time. You can use `setQueryLowerBound` to help Reach know at what block number to start querying, so that it does not have to start querying at the beginning of time, which can take quite a while.

How can I use ConfluxPortal with the Reach devnet?

If you find that ConfluxPortal's Localhost 12537 default configuration does not work correctly with Reach apps, you can try configuring ConfluxPortal to use a custom RPC endpoint:

1. Click the network dropdown in Conflux Portal
1. Select: Custom RPC
1. Use RPC url: `http://127.0.0.1:12537`

If your locally-running Conflux devnet restarts, you may find that you need to reset ConfluxPortal's account history, which you can do like so:

1. Select the desired account
1. Click the profile image of the account (top-right)
1. Click Settings > Advanced > Reset Account > (confirm) Reset
1. Switch to a different network and back
1. CTRL+SHIFT+R to hard-reset the webpage.
