#lang scribble/manual
@(require "lib.rkt")

@title[#:version reach-vers #:tag "guide-ganache"]{How to use Ganache with Reach}

@link["https://www.trufflesuite.com/ganache"]{Ganache} is a very popular Ethereum personal blockchain for quick application development, deployment, and testing with a replica of the Ethereum mainnet.
Ganache provides many customization options, such as control over the gas limit and price.

There are two ways to use Ganache with Reach.

First, you can run the Ganache desktop application and start a workspace, then connect to it using the Reach @litchar{ETH-live} connector mode.
This requires specifying the node URI as well.
For example, if you're running your Reach frontend from within Docker, and Ganache on your host machine, you might run:
@cmd{
  REACH_CONNECTOR_MODE=ETH-live ETH_NODE_URI=http://host.docker.internal:7545 REACH_ISOLATED_NETWORK=1 reach run
}
This sets the Ethereum URI to the host machine's port and instructs Reach that the network is "isolated", which means it is for testing.

Second, you can programmatically create a Ganache network from inside of your JavaScript frontend and set it is as a provider for the Reach standard library.
This requires some modification to your frontend.

You need to add the @litchar{ganache-core} package as a dependency in your @litchar{package.json} file.
If you don't already have one, then you'll want to use @exec{reach scaffold} to set one up.
You'll also need to add the @litchar{ethers} package, because you'll be directly interacting with the underlying network to set things up.

After that, you need to import the two packages on the JavaScript side in your @exec{index.mjs} file:
@reachex["ganache/index.mjs" 'only 3 4 "// ..."]

Next, you need to actually create the Ganache-based provider and connect it to Reach:
@reachex["ganache/index.mjs" 'only 9 13 "  // ..."]

This will work, but Reach will not consider the network to be "isolated", which means that it won't be able to fund test accounts.
You can enable this by setting the Reach faucet:
@reachex["ganache/index.mjs" 'only 15 16 "  // ..."]

If you want to use some of the interesting customizations that Ganache provides, then refer to @link["https://github.com/trufflesuite/ganache/tree/master#options"]{their documentation} about what @jsin{ganacheOptions} may be set to.
