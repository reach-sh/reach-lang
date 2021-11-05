Build dApps with Reach

Alice and Bob are now ready to kick-off development of their Algorand-powered auction dApp! This guide will take you through the steps to build the dApp, which includes setting up a development environment, writing the dApp code which can contain multiple smart contracts, deploying it, and writing the functions to interact with it.

A few tips before getting started. The goal of this guide is to get you up and running with a working prototype that represents a real use case, as quickly as possible. We use a hands-on example to teach you basic design principles and best practices for building dApps with Reach on Algorand. This guide does not cover all the details of the dApp backend and frontend code. This is intentional so that you can focus on solidifying higher-level concepts that will be the foundation for building any dApp on Algorand using Reach. So don’t worry if you don’t understand what everything does in the solution. This is expected! After you feel comfortable with the basics, you can head over to the [Reach documentation](https://docs.reach.sh/) and work on becoming an expert in the Reach dApp language.

Now let’s get started. 

# Organization

This guide is organized into two sections. The first section helps you launch the dApp and run an auction simulation. The second section provides a deeper dive into the different components of the auction application.

All of the code for this guide is located here [Link to Auction](https://github.com/TheChronicMonster/reach-lang/tree/master/examples/simple-nft-auction). Download index.mjs and index.rsh and follow along!

## Install Reach

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed. The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop).


To confirm everything is installed try to run the following three commands and see no errors
  $ make --version
  $ docker --version
  $ docker-compose --version
If you’re using Windows, consult [the guide to using Reach on Windows](https://docs.reach.sh/guide-windows.html).
Once confirmed that reach prerequisite are installed, choose a directory for this project such as:

``` bash
  $ mkdir -p ~/reach/auction && cd ~/reach/auction
```

Next, download Reach by running

``` bash
  $ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Confirm the download worked by running

``` bash
  $ ./reach version
```

Since Reach is Dockerized, when first used, the images it uses need to be downloaded. This will happen automatically when used for the first time, but can be done manually now by running

``` bash
  $ ./reach update
```

You’ll know that everything is in order if you can run

``` bash
  $ ./reach compile --help
```

To determine the current version is installed, run

``` bash
  $ ./reach hashes
```

Output should look similar to:

``` bash
reach: fb449c94
reach-cli: fb449c94
react-runner: fb449c94
rpc-server: fb449c94
runner: fb449c94
devnet-algo: fb449c94
devnet-cfx: fb449c94
devnet-eth: fb449c94
```

All of the hashes listed should be the same and then visit the #releases channel on the [Reach Discord Server](https://discord.gg/9kbHPfwbwn) to see the current hashes.

More information: Detailed Reach install instructions can be found in the [reach docs](https://reach.sh/). 

## Clone the Reach Auction demo application

Clone the repository using the following commands.

```bash
// TBD git clone [insert link] 

```

## Setup environments and run tests

Navigate to your project folder

``` bash
cd  ~/reach/auction
```

Sometimes it may be convenient to use the reach run command, preceded by setting the REACH_CONNECTOR_MODE , especially when testing multiple blockchain deployments.

``` bash
  REACH_CONNECTOR_MODE=ALGO-devnet ./reach run
```

Set an environment variable to use the Algorand Blockchain. 

``` bash
export REACH_CONNECTOR_MODE="ALGO-devnet"
```

# Application OverView

Alice and Bob’s auction design

The sample app shown below is an auction. Alice and Bob think through the features for their dApp. They list off the following requirements:

1. Sellers must be able to create a new auction for each piece of artwork. The artwork must be held by the contract after the auction begins and until the auction closes.
1. The auction can be closed before it begins, in which case the artwork will be returned to the seller.
Sellers can specify a reserve price, which if not met, will return the artwork to them.
1. For each bid, if the new bid is higher than the previous bid, the previous bid will be refunded to the previous bidder and the new bid will be recorded and held by the contract.
1. At the end of a successful auction, where the reserve price was met, the highest bidder will receive the artwork and the seller will receive the full bid amount.

Copy these two files into your  ~/reach/auction folder.

_index.rsh_ is the Backend which provides the implementation of the solution in. It also determines what is published to the blockchain and how. It also defines the interfaces to the frontend.

_index.mjs_ is the Frontend provides a User Interface including prompts and a web and/or mobile app  frontend. It creates accounts and provides the logic of `interact` methods.

# The dApp

A build folder is created in your project by the Reach compiler when using `./reach run`. 

This folder contains a file called _index.main.mjs_. The file contains an asynchronous function for each participant. For example, if a Reach program contains a participant named 'Alice' in the Reach.App, then the JavaScript backend will include a function named Alice (i.e. backend.Alice). The Promise returned by these functions is resolved when the Reach program terminates (i.e. reaches exit();). 

Run the auction

``` bash
$ ./reach run
```

Output should be similar to below,  showing a winner of the auction:

``` highlight
Alice has 10 ALGO and 0 of the NFT
Alice decides to bid 2.508057
Bob has 10 ALGO and 0 of the NFT
Bob decides to bid 9.575844
Carla has 10 ALGO and 0 of the NFT
Carla decides to bid 3.81785
Creator has 9.999 ALGO and 1 of the NFT
Creator sets parameters of sale
Bob sees that the NFT is 6, the reserve price is 2, and that they have until 38 to bid
Alice sees that the NFT is 6, the reserve price is 2, and that they have until 38 to bid
Carla sees that the NFT is 6, the reserve price is 2, and that they have until 38 to bid
Bob bids 9.575844 against 2
Alice bids 2.508057 against 2
Carla bids 3.81785 against 2
Alice does not bid because 9.575844 is too high
Carla does not bid because 9.575844 is too high
Creator saw that 2KRZLQILBKY43A5OC56TP3SJBBSP35MV5X4GSVZDUOHYYEQK2JU5TN27BI bid 9.575844
Creator observes the auction has hit the timeout
Creator observes the auction has hit the timeout
Creator saw that 2KRZLQILBKY43A5OC56TP3SJBBSP35MV5X4GSVZDUOHYYEQK2JU5TN27BI won
Creator has 19.561862 ALGO and 0 of the NFT
Bob saw that 2KRZLQILBKY43A5OC56TP3SJBBSP35MV5X4GSVZDUOHYYEQK2JU5TN27BI won
Bob has 0.421156 ALGO and 1 of the NFT
Alice does not bid because 9.575844 is too high
Carla does not bid because 9.575844 is too high
Alice saw that 2KRZLQILBKY43A5OC56TP3SJBBSP35MV5X4GSVZDUOHYYEQK2JU5TN27BI won
Carla saw that 2KRZLQILBKY43A5OC56TP3SJBBSP35MV5X4GSVZDUOHYYEQK2JU5TN27BI won
Alice has 9.999009 ALGO and 0 of the NFT
Carla has 9.999009 ALGO and 0 of the NFT
```

## Basic Functions of the auction

The Backend, _index.rsh_, defines the interface for functions coded in the frontend. The Creator  is a Participant that has getSale, seeBid and timeout functions.  A participant is an “actor” which takes part in the application (dApp). A participant is associated with an account (address) on the consensus network. A participant can have persistently stored values, called its local state. 

The Bidder is a ParticipantClass that has seeParams and getBid functions. A participant class is a category of Participant, it is like a Participant… but can occur many times in a single application. Example: an application where users vote for their favorite puppy. There can be many voters voting, but they are all voters. All voters would be a member of the “voter participant class”. In the auction dApp we have a participant class of bidders, with each bidder having the ability to place a bid.  

``` js
'reach 0.1';

const MUInt = Maybe(UInt);
const common = {
 showOutcome: Fun([Address], Null)
};
const Params = Tuple(Token, UInt, UInt);

export const main = Reach.App(() => {
 const Creator = Participant('Creator', {
   ...common,
   getSale: Fun([], Params),
   seeBid: Fun([Address, UInt], Null),
   timeout: Fun([], Null),
 });
 const Bidder = ParticipantClass('Bidder', {
   ...common,
   seeParams: Fun([Params], Null),
   getBid: Fun([UInt], MUInt),
 });
 deploy();
```

The functions in the frontend are called from the backend using the interact interface. Notice in the code below, `interact.getSale()` which returns nftID, reservePrice and lenInBlocks.  The Creator of the auction publishes the auction params for NFT id,  reservePrice and lenInBlocks and commits to the blockchain. The Bidder sees the params. If the entire DApp is waiting for a single participant to act, such as when at a play the entire theatre waits in anticipation for the stage hands to draw the curtains, then you either need a pay or publish. If the single participant is sharing information, then you need a publish; but if they are only paying a previously known amount, then you need a pay. This kind of transfer always explicitly names the party acting, as in:

Publish Information

``` js
Creator.publish(nftId, reservePrice, lenInBlocks);
```

Pay and publish without a race are for when one participant wants to do one thing.
Make a pay transaction

``` js
Creator.pay([[amt, nftId]]);
 Creator.only(() => {
   const [ nftId, reservePrice, lenInBlocks ] = declassify(interact.getSale());
 });
 Creator.publish(nftId, reservePrice, lenInBlocks);
 const amt = 1;
 commit();
 Creator.pay([[amt, nftId]]);
 const end = lastConsensusTime() + lenInBlocks;
 Bidder.interact.seeParams([nftId, reservePrice, end]);
```

A Consensus Network is a Network protocol that contains network tokens (ALGO, ETH, etc.), non-network tokens (ASA, ERC-20, etc.), as well as a set of accounts and contracts. 
Rules for the outcome of the bidding are next. The consensus transfer uses parallelReduce, which facilitates bidders repeatedly providing new bids as they compete to be the highest bidder before a time limit is reached. Additional consensus transfer patterns are discussed in the [reach documentation](https://docs.reach.sh/). 
 
``` js
 const [ highestBidder, lastPrice, currentPrice ] =
   parallelReduce([ Creator, 0, reservePrice ])
     .invariant(balance(nftId) == amt && balance() == lastPrice)
     .while(lastConsensusTime() <= end)
     .case(Bidder,
       (() => {
         const mbid = highestBidder != this
           ? declassify(interact.getBid(currentPrice))
           : MUInt.None();
         return ({
           when: maybe(mbid, false, ((b) => b > currentPrice)),
           msg : fromSome(mbid, 0)
         });
       }),
       ((bid) => bid),
       ((bid) => {
         require(bid > currentPrice);
         transfer(lastPrice).to(highestBidder);
         Creator.interact.seeBid(this, bid);
         return [ this, bid, bid ];
       }))
```     

Maybe can be some or none: (evaluate the return of a function)   The transfer is made from the Bidder to the Creator for the bid amount lastPrice on the NFT. The NFT is transferred to the highest bidder.  Each can see the results with showOutcome. 

``` js
 transfer(lastPrice).to(Creator);
 transfer(amt, nftId).to(highestBidder);
 commit();

 each([Creator, Bidder], () => interact.showOutcome(highestBidder));
 exit();
});
```

The Frontend, _index.mjs_, Create test accounts for Alice, Bob, and Carla with a balance of 100 algos. The Creator deploys the contract. 

``` js
import { loadStdlib } from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const N = 3;
const names = ["Creator", "Alice", "Bob", "Carla"];

(async () => {
 const stdlib = await loadStdlib(process.env);
 const startingBalance = stdlib.parseCurrency(10);
 const [ accCreator, ...accBidders ] =
   await stdlib.newTestAccounts(1+N, startingBalance);
 // We're including this for automation, but it would be better if the NFT is
 // assumed to already exist, or if it this contract actually created it.
 const theNFT = await stdlib.launchToken(accCreator, "beepboop", "NFT", { supply: 1 });

 await Promise.all( [ accCreator, ...accBidders ].map(async (acc, i) => {
   acc.setDebugLabel(names[i]);
 }));

 const ctcCreator = accCreator.contract(backend);
```

Provide the  logic for the `interact` methods. Interact methods are called from the backend for the frontend to execute. Functions in _index.mjs_ include: showBalance, getSale, seeBid, timeout, showOutcome and showBalance. 

``` js
 const showBalance = async (acc, i) => {
   const amt = await stdlib.balanceOf(acc);
   const amtNFT = await stdlib.balanceOf(acc, theNFT.id);
   console.log(`${names[i]} has ${stdlib.formatCurrency(amt)} ${stdlib.standardUnit} and ${amtNFT} of the NFT`);
 };

 await Promise.all([
   (async () => {
     await showBalance(accCreator, 0);
     const n = names[0];
     await backend.Creator(ctcCreator, {
       getSale: () => {
         console.log(`${n} sets parameters of sale`);
         return [ theNFT.id, stdlib.parseCurrency(2), 30 ]
       },
       seeBid: (who, bid) => {
         console.log(`${n} saw that ${stdlib.formatAddress(who)} bid ${stdlib.formatCurrency(bid)}`);
       },
       timeout: () => {
         console.log(`${n} observes the auction has hit the timeout`);
       },
       showOutcome: (winner) => {
         console.log(`${n} saw that ${stdlib.formatAddress(winner)} won`);
       },
     });
     await showBalance(accCreator, 0);
   })(),
```

The bidders attach (opt in) to the contract that the Creator deployed. Console messages are displayed for the bidding process. 

``` js
   ...accBidders.map(async (acc, i) => {
     await showBalance(acc, i+1);
     const n = names[i+1];
     const ctc = acc.contract(backend, ctcCreator.getInfo());
     const bid = stdlib.parseCurrency(Math.random() * 10);
     let IWon = false;
     console.log(`${n} decides to bid ${stdlib.formatCurrency(bid)}`);
     await backend.Bidder(ctc, {
       showOutcome: (winner) => {
         console.log(`${n} saw that ${stdlib.formatAddress(winner)} won`);
         IWon = stdlib.addressEq(winner, acc);
       },
       seeParams: async ([nftId, reservePrice, end]) => {
         console.log(`${n} sees that the NFT is ${nftId}, the reserve price is ${stdlib.formatCurrency(reservePrice)}, and that they have until ${end} to bid`);
         await acc.tokenAccept(nftId);
       },
       getBid: (currentPrice) => {
         if ( currentPrice.lt(bid) ) {
           console.log(`${n} bids ${stdlib.formatCurrency(bid)} against ${stdlib.formatCurrency(currentPrice)}`);
           return ['Some', bid];
         } else {
           console.log(`${n} does not bid because ${stdlib.formatCurrency(currentPrice)} is too high`);
           return ['None', null];
         }
       },
     });
     await showBalance(acc, i+1);
     if ( ! IWon ) {
       await theNFT.optOut(acc);
     }
     return;
   },
 )]);
})();
```

## Verification

Verification is a key feature to Reach. It is facilitated by the use of assert statements. For more details on verification, auditing, mathematical proofs, cryptographic commitment schemes and timeouts see the Reach [documentation](https://docs.reach.sh/guide-assert.html). [For the technical among us, dig deeper into verification with loop invariants](https://docs.reach.sh/guide-loop-invs.html).
 
## Remote Procedure Calls (RPC)

The Reach RPC Server provides access to compiled JavaScript backends via an HTTPS-accessible JSON-based RPC protocol. The server allows frontends to be written in any programming language. Reach provides client libraries for JavaScript, Python, and Go. It is easy to implement a client library yourself. Example frontends written using the Reach RPC Server can be found in the get details Reach page. [link](https://docs.reach.sh/ref-backends-rpc.html)

# Summary

Reach builds distributed Apps (dApps) and deploys the entire Application, not just the smart contract. This completes the guide to launch Alice’s auction dApp on Algorand. In summary, this document provides how to set up the development environment, create a sample dApp and deploy it to the blockchain. Reach has backend and frontend components and has the ability to verify the dApp. Now that you know the fundamentals. You can jump further and get details of [smart contract development using Reach](https://docs.reach.sh/guide-solidity.html) which provides Remote Procedure Call (RPC) Frontend in Python, Go and JavaScript as well as deploying to Algorand TestNet and MainNet.

Complete code for this auction simulation can be found [here](). 
