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

```js
29    Owner.only(() => {
30        const { nftId } = declassify(interact.setNFT());
31    });

33    Owner.publish(nftId);
34    commit();
```

- Line 29 to 31 is the owner participant interacting with the setNFT function from the frontend. It returns the nftId provided by the owner participant.
- Line 33 is used to broadcast/publish the following inputs by the owner participant to the blockchain so it can be seen that the owner has set the nftId to be auctioned
- Line 34 signifies the end of the current consensus step.

```js
36    Auctioneer.only(() => {
37        const { minPrice, minBidDiff, lengthInBlocks } = declassify(interact.startAuction());
38    });


41    Auctioneer.publish(minPrice, minBidDiff, lengthInBlocks);
42    const nftAmt = 1;
43    commit();

```
- Line 36 to 38: The Auctioneer interacts with the application, starting the auction by providing the miniumum starting price for the auction `minPrice`, The minimum  price difference allowed between the current and next bid `minBidDiff` and the length of the auction through the `lengthInBlocks` variable.
- Line 41 publishes/broadcasts the inputs from the Auctioneer to the blockchain
- Line 42 creates a constant that sets the quantity of the NFT to be auctioned
- Line 43 signifies the end of the current consensus step

```js
45    Owner.pay([[nftAmt, nftId]]);
46    assert(balance(nftId) == nftAmt, "balance of NFT is wrong");
47    const [timeRemaining, keepGoing] = makeDeadline(lengthInBlocks);

49    var [highestBidder, lastPrice, isFirstBid] = [Auctioneer, minPrice, true];

51    invariant(balance(nftId) == nftAmt);
52    invariant(balance() == (isFirstBid ? 0 : lastPrice));
```

- On line 45 the owner pays the quantity of NFT into the contract
- Line 46 checks to make sure the nft in the contract has the right quantity
- Going to Line 47, we create an absolute deadline using the `makeDeadline` function. The function takes the `lengthINBloocks` value set by the auctioneer as a parameter. It returns to values `timeRemaining` and a boolean value `keepGoing`.


- On line 49, we set the initial values for the auction. The `highestBidder` is set to the `Auctioneer` object, `lastPrice` is set to the `minPrice` specified and a boolean `isFirstBid` set to `true`
- Line 51 we define an invariant expression. It is an expression that must be true for each iteration of the while loop. If this expression returns false at any point, it returns a theorem violation error.
- We also have another invariant expression on Line 52 that checks if the current balance is 0 for the first bid and the last bid price for subsequent bids

Let's define the actual auction interaction for the bidders. The auction will continue to run as bidders bid for the nft throughout the duration set by the auctioneer. This will be made possible with the help of a while loop.

In reach, the syntax to follow while using a while loop is stated below

var LHS = INIT_EXPR;
DEFINE_BLOCK; // optional
invariant(INVARIANT_EXPR, ?INVARIANT_MSG);
while( COND_EXPR ) BLOCK

```js
    while (keepGoing()) {
        commit();
        fork()
            .api_(Bidder.bid,
                (bid) => {
                    check(bid > lastPrice, "bid is too low");
                    check(sub(bid, lastPrice) >= minBidDiff, "bid difference is too low");

                    return [bid, (notify) => {
                        notify([highestBidder, lastPrice]);

                        if (!isFirstBid) {
                            transfer(lastPrice).to(highestBidder)
                        }

                        each([Owner, Auctioneer], () => {
                            interact.seeBid(this, bid);
                        });

                        [highestBidder, lastPrice, isFirstBid] = [this, bid, false];


                    }];
                }
            );

        continue;
    }

```