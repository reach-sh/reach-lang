<p align="center">
  <a href="" rel="noopener">
 <img src="https://docs.reach.sh/assets/logo.png" alt="Project logo"></a>
</p>
<h3 align="center">NFT Timed Auction</h3>

<div align="center">


</div>

---

<p align="center"> NFT Timed Auction
    <br> 
</p>

In this tutorial, we would go throug all the steps for creating a simple decentralized NFT auction application on the algorand blockchain using the REACH framework

This tutorial is for all skill levels, both new and experienced developers in the blockchain space.

Lets start by answering a few questions

- What is blockchain ?
- What is a decentralized application ?
- What is Reach ?
- What is an NFT ?
- Auctioning

## Introduction



## Installation and Configurations


## Setup

At the end of this tutorial you will be able to create a decentralized GUI app on the algorand developer network using the reach rpc server and react

Lets start with the `index.rsh` file which is the backend file and also the smart contract of this NFT timed auction app. In this application we have an individual acting as the NFT Owner, an Auctioneer and Bidders. It is much better to follow along and write the code than just copying and pasting. Let's dive in.

```js
1 "reach 0.1";

3 export const main = Reach.App(() => {

})

3 export const main = Reach.App(() => {

5        const Owner = Participant('Owner', {
6        setNFT: Fun([], Object({
7            nftId: Token
8        })),
9        seeBid: Fun([Address, UInt], Null),
10       showOutcome: Fun([Address, UInt], Null)
11   });
12})
```
- Line 1 tells the compiler that this is a reach file.
- Line 3 defines the starting point of the reach application
- Line 5 to 11 defines the owner participant interface with the ability to set the NFT, see the latest bids, and show the outcome of the auction.
- Line 6 `setNFT: Fun([], Object({nftId: Token})),` is a function that takes in no parameter and returns an object nftId key-value pair
- Line 9 `seeBid: Fun([Address, UInt], Null)` is a function that returns both Address and UInt parameters to the frontend. The function is used see the current bid that was placed. The `Address` parameter has details about the current bidder, and the `UInt` parameter has details about the bid price placed by the bidder.
- Line 10 `showOutcome: Fun([Address, UInt], Null)` is a function that returns the highest bidder and highest bid to the frontend when the auction is over.

Let's define the Auctioneer participant interface

```js
13    const Auctioneer = Participant('Auctioneer', {
14      startAuction: Fun([], Object({
15            minPrice: UInt,
16            minBidDiff: UInt,
17            lengthInBlocks: UInt
18        })),
19        seeBid: Fun([Address, UInt], Null),
20        showOutcome: Fun([Address, UInt], Null)
21    });

```

Line 13 to 21 defines the Auctioneer interface

- Line 14 `startAuction: Fun([], Object({minPrice: UInt,minBidDiff: UInt,lengthInBlocks: UInt})),` is a function that returns an object from the frontend with minPrice, minBidDiff, lenghtInBlocks key-value pairs.
- Just like the Owner interface
- Line 19 `seeBid: Fun([Address, UInt], Null)` is a function that returns both Address and UInt parameters to the frontend. The function is used see the current bid that was placed. The `Address` parameter has details about the current bidder, and the `UInt` parameter has details about the bid price placed by the bidder.
- Line 20 `showOutcome: Fun([Address, UInt], Null)` is a function that returns the highest bidder and highest bid to the frontend when the auction is over.

Let's move on to defining the Bidder API

```js

23    const Bidder = API('Bidder', {
24        bid: Fun([UInt], Tuple(Address, UInt))
25    })

27    init();

```
- Line 23 to 25 defines the Bidder API interface with a single function
- Line 24 ` bid: Fun([UInt], Tuple(Address, UInt))` is a function that takes in a bid price parameter from the frontend and returns the Address of the highest Bidder and the Last bid price
- Line 27 is used to initialize the application and finalize all the available participants and API interfaces

Next we would start by looking at an interact object