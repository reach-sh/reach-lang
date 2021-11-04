# Build a Simple NFT dApps with Reach

Alice and Bob are now ready to kick-off development of their auction dApp! This guide will take you through the steps to build the dApp, which includes setting up a development environment, writing the dApp code which can contain multiple smart contracts, deploying it, and writing the functions to interact with it.

A few tips before getting started. The goal of this guide is to get you up and running with a working prototype that represents a real use case, as quickly as possible. We use a hands-on example to teach you basic design principles and best practices for building dApps with Reach. This guide does not cover all the details of the dApp backend and frontend code. This is intentional so that you can focus on solidifying higher-level concepts that will be the foundation for building any dApp on Reach. So don’t worry if you don’t understand what everything does in the solution. This is expected! After you feel comfortable with the basics, you can head over to the Reach documentation and work on becoming an expert in the Reach dApp language.

## Setup environments and run tests

Navigate to your project folder

`cd  ~/reach/auction`

Sometimes it may be convenient to use the reach run command, preceded by setting the REACH_CONNECTOR_MODE , especially when testing multiple blockchain deployments.

`  REACH_CONNECTOR_MODE=ALGO-devnet ./reach run`

Set an environment variable to use the Algorand Blockchain. 

`export REACH_CONNECTOR_MODE="ALGO-devnet"`

# Application OverView

Alice and Bob’s auction design
The sample app shown below is an auction. Alice and Bob think through the features for their dApp. They list off the following requirements:
Sellers must be able to create a new auction for each piece of artwork. The artwork must be held by the contract after the auction begins and until the auction closes.
The auction can be closed before it begins, in which case the artwork will be returned to the seller.
Sellers can specify a reserve price, which if not met, will return the artwork to them.
For each bid, if the new bid is higher than the previous bid, the previous bid will be refunded to the previous bidder and the new bid will be recorded and held by the contract.
At the end of a successful auction, where the reserve price was met, the highest bidder will receive the artwork and the seller will receive the full bid amount.
Copy these two files into your  ~/reach/auction folder.

index.rsh  is the Backend which provides the implementation of the solution in. It also determines what is published to the blockchain and how. It also defines the interfaces to the frontend.

index.mjs is the Frontend provides a User Interface including prompts and a web and/or mobile app  frontend. It creates accounts and provides the logic of `interact` methods.

# The dApp

A build folder is created in your project by the Reach compiler when using `./reach run`. 

This folder contains a file called index.main.mjs. The file contains an asynchronous function for each participant. For example, if a Reach program contains a participant named 'Alice' in the Reach.App, then the JavaScript backend will include a function named Alice (i.e. backend.Alice). The Promise returned by these functions is resolved when the Reach program terminates (i.e. reaches exit();). 

Run the auction

`$ ./reach run`

Output should be similar to below, showing a winner of the auction:

```
Carla has 10 ALGO and 0 of the NFT
Carla decides to bid 2.473848
Alice has 10 ALGO and 0 of the NFT
Alice decides to bid 2.759044
Creator has 9.999 ALGO and 1 of the NFT
Creator sets parameters of sale
Bob has 10 ALGO and 0 of the NFT
Bob decides to bid 9.552626
Bob sees that the NFT is 61, the reserve price is 2, and that they have until 82 to bid
Carla sees that the NFT is 61, the reserve price is 2, and that they have until 82 to bid
Alice sees that the NFT is 61, the reserve price is 2, and that they have until 82 to bid
Bob bids 9.552626 against 2
Alice bids 2.759044 against 2
Carla bids 2.473848 against 2
Alice bids 2.759044 against 2.473848
Creator saw that NQF4VHIMYXPDOUNZACV43H6W2ZNTO4DVUF4FR47DOLTWHB3MHSH5KRHRPY bid 2.473848
Bob bids 9.552626 against 2.473848
Carla does not bid because 2.759044 is too high
Creator saw that YTCSVRPHVB4P4PTM2JJ4GQMQCOBYFIDPYXB3KVHJ3JHWTBUPRTSN2VTOLE bid 2.759044
Bob bids 9.552626 against 2.759044
Creator observes the auction has hit the timeout
Creator observes the auction has hit the timeout
Bob bids 9.552626 against 2.759044
Carla does not bid because 2.759044 is too high
Creator saw that YTCSVRPHVB4P4PTM2JJ4GQMQCOBYFIDPYXB3KVHJ3JHWTBUPRTSN2VTOLE won
Creator has 12.745055 ALGO and 0 of the NFT
Carla saw that YTCSVRPHVB4P4PTM2JJ4GQMQCOBYFIDPYXB3KVHJ3JHWTBUPRTSN2VTOLE won
Carla has 9.997009 ALGO and 0 of the NFT
Alice saw that YTCSVRPHVB4P4PTM2JJ4GQMQCOBYFIDPYXB3KVHJ3JHWTBUPRTSN2VTOLE won
Alice has 7.236963 ALGO and 1 of the NFT
Bob saw that YTCSVRPHVB4P4PTM2JJ4GQMQCOBYFIDPYXB3KVHJ3JHWTBUPRTSN2VTOLE won
Bob has 9.999009 ALGO and 0 of the NFT
```
## Basic Functions of the auction

The Backend, index.rsh, defines the interface for functions coded in the frontend. The Creator  is a Participant that has getSale, seeBid and timeout functions.  A participant is an “actor” which takes part in the application (dApp). A participant is associated with an account (address) on the consensus network. A participant can have persistently stored values, called its local state. 

The Bidder is a ParticipantClass that has seeParams and getBid functions. A participant class is a category of Participant, it is like a Participant… but can occur many times in a single application. Example: an application where users vote for their favorite puppy. There can be many voters voting, but they are all voters. All voters would be a member of the “voter
participant class”. In the auction dApp we have a participant class of bidders, with each bidder having the ability to place a bid.  

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

The functions in the frontend are called from the backend using the interact interface. Notice in the code below, `interact.getSale()` which returns nftID, reservePrice and lenInBlocks. The Creator of the auction publishes the auction params for NFT id, reservePrice and lenInBlocks and commits to the blockchain. The Bidder sees the params. If the entire DApp is waiting for a single participant to act, such as when at a play the entire theatre waits in anticipation for the stage hands to draw the curtains, then you either need a pay or publish. If the single participant is sharing information, then you need a publish; but if they are only paying a previously known amount, then you need a pay. This kind of transfer always explicitly names the party acting, as in:

Publish Information

`Creator.publish(nftId, reservePrice, lenInBlocks);`
 
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
Rules for the outcome of the bidding are next. The consensus transfer uses parallelReduce, which facilitates bidders repeatedly providing new bids as they compete to be the highest bidder before a time limit is reached. Additional consensus transfer patterns are discussed in the reach documentation here. 

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

Maybe can be some or none: (evaluate the return of a function) The transfer is made from the Bidder to the Creator for the bid amount lastPrice on the NFT. The NFT is transferred to the highest bidder. Each can see the results with showOutcome. 

``` js
 transfer(lastPrice).to(Creator);
 transfer(amt, nftId).to(highestBidder);
 commit();

 each([Creator, Bidder], () => interact.showOutcome(highestBidder));
 exit();
});
```

The Frontend, _Index.mjs_, Create test accounts for Alice, Bob, and Carla with a balance of 100 algos. The Creator deploys the contract. 

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

Provide the logic for the `interact` methods. Interact methods are called from the backend for the frontend to execute. Functions in index.mjs include: showBalance, getSale, seeBid, timeout, showOutcome and showBalance. 

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
