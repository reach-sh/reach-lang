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

Lets start with the `index.rsh` file which is the backend file and also the smart contract of this NFT timed auction app. In this application we have an individual acting as the NFT Owner, an Auctioneer and Bidders.

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

where `LHS` is a valid left-hand side of an identifier definition where the expression `INIT_EXPR` is the right-hand side, and  `DEFINE_BLOCK` is an optional block that may define bindings that use the `LHS` values which are bound inside the rest of the while and its tail, and  `INVARIANT_EXPR` is an expression, called the loop invariant, that must be true before and after every execution of the block `BLOCK`—it may be specified multiple times, `INVARIANT_MSG` is an optional bytes argument, which is included in any reported violation, and if `COND_EXPR` is true, then the block executes, and if not, then the loop terminates and control transfers to the continuation of the while statement. The identifiers bound by LHS are bound within `DEFINE_BLOCK`, `INVARIANT_EXPR`, `COND_EXPR`, `BLOCK`, and the tail of the while statement.


```js
54    while (keepGoing()) {
55        commit();
56        fork()
57            .api_(Bidder.bid,
58                (bid) => {
59                    check(bid > lastPrice, "bid is too low");
60                    check(sub(bid, lastPrice) >= minBidDiff, "bid difference is too low");
61
62                    return [bid, (notify) => {
63                        notify([highestBidder, lastPrice]);
64
65                        if (!isFirstBid) {
66                           transfer(lastPrice).to(highestBidder)
67                        }
68
69                        each([Owner, Auctioneer], () => {
70                            interact.seeBid(this, bid);
71                        });
72
73                        [highestBidder, lastPrice, isFirstBid] = [this, bid, false];
74
75
76                    }];
77                }
78            );
79
80        continue;
81    }

```

- On Line 56, we define a fork statement. From the official documentation - "A fork statement is an abbreviation of a common race and switch pattern you could write yourself."
- From Line 57, we chain an `api_` component to the fork statment. The `api_` component listens for the bid function call from the frontend, with access to the returned bid.
- Line 59 uses a check statement "A dynamic assertion that claim evaluates to true, which expands to either a require or assume depending on where it is used in a program. It accepts an optional bytes argument, which is included in any reported violation."
- The check statement, ensures the current bid is greater than the last price `lastPrice` bidded for the NFT
- We also write another check statement to make sure the difference between the current bid `bid` and last price `lastPrice` is greater than or equal to the minimum bid difference `minBidDiff` set by the Auctioneer participant.
- The `api_` statement is required to return an `API_CHECKED_CONSENSUS_EXPR` "A function parameterized over the input to the API member function" according to the documentation.
- The function takes the current bid placed by the `API` individual as the first parameter, and a function as the second parameter
- The second function parameter is parameterized with the `notify` expression.
- On Line 63, we make use of the notify expression. The notify expression takes an array as an argument. The elements of the array must match the return values of the `bid` function set on the Bidder API expression. The notify expression returns the values specified in its block to the frontend anytime the `bid` function is called with no errors.

```js
65   if (!isFirstBid) {
66      transfer(lastPrice).to(highestBidder)
67   }

```

- Line 65 to 67, we use an if statement to check if the bid is the first bid placed. If it's not the first bid, the last bidder is refunded his bid amount

```js
69   each([Owner, Auctioneer], () => {
70      interact.seeBid(this, bid);
71   });

```

- From Line 69 to 71, we use the each statement to loop through each participant passed in the array argument, and call the `interact.seeBid` function defined in both the `Owner` and `Auctioneer` Participant interface. Line 69 to 71 allows the Owner and Auctioneer interact with the `seeBid` function.

```js
73   [highestBidder, lastPrice, isFirstBid] = [this, bid, false];
```

- On line 73, we change the values specified on the `LHS` to its corresponding new values.
- the `highestBidder` variable is set to the `this` keyword which is available in the `api_` statement. The `this` keyword returns the address of the API individually calling the particular function, in this case `bid`
- The `lastPrice` variable is set to the latest bid placed by `this`
- While `isFirstBid` is always set to false after every bid.


```js
83    commit();
84    Owner.publish();

86    transfer(nftAmt, nftId).to(highestBidder);

```

- At the end of the auction (deadline), all consensus step will be ended by calling the commit expression on Line 83
- The Owner publishes and boradcasts all changes on the blockchain
- And on Line 86, The NFT with the number of quantity specified is transfered to the wallet of the highest bidder `transfer(nftAmt, nftId).to(highestBidder);`


```js
88    const auctioneerFee = div(mul(lastPrice, 1), 100);
89    const ownerMoney = sub(lastPrice, auctioneerFee);

```

- On Line 88, we calculate the 1% fee from the last bid price `lastPrice` that will be transfered to the auctioneer
- The total price going to the NFT owner will be the difference between the `lastPrice` and the `auctioneerFee`


```js
91    if (!isFirstBid) {
92        transfer(ownerMoney).to(Owner);
93        transfer(auctioneerFee).to(Auctioneer);
94    }
```

- From Line 91 to 94, the application checks if the `isFirstBid` variable is false. If it returns false, the `ownerMoney` is transferred to the `Owner` Participant, while the `auctioneerFee` is transferred to the `Auctioneer` Participant


```js
96    each([Auctioneer, Owner], () => {
97        interact.showOutcome(highestBidder, lastPrice);
98    });
99    commit();
100    exit();

```

- Line 96 to 98 allows the `Auctioneer` and `Owner` Participant interact with the `showOutcome` function
- Line 100 signifies the end of the smart contract and the NFT timed auction application.

Let's take a look at our final backend code `index.rsh`

```js
'reach 0.1'

export const main = Reach.App(() => {

    const Owner = Participant('Owner', {
        setNFT: Fun([], Object({
            nftId: Token
        })),
        seeBid: Fun([Address, UInt], Null),
        showOutcome: Fun([Address, UInt], Null)
    });

    const Auctioneer = Participant('Auctioneer', {
        startAuction: Fun([], Object({
            minPrice: UInt,
            minBidDiff: UInt,
            lengthInBlocks: UInt
        })),
        seeBid: Fun([Address, UInt], Null),
        showOutcome: Fun([Address, UInt], Null)
    });

    const Bidder = API('Bidder', {
        bid: Fun([UInt], Tuple(Address, UInt))
    })

    init();

    Owner.only(() => {
        const { nftId } = declassify(interact.setNFT());
    });

    Owner.publish(nftId);
    commit();

    Auctioneer.only(() => {
        const { minPrice, minBidDiff, lengthInBlocks } = declassify(interact.startAuction());
    });


    Auctioneer.publish(minPrice, minBidDiff, lengthInBlocks);
    const nftAmt = 1;
    commit();

    Owner.pay([[nftAmt, nftId]]);
    assert(balance(nftId) == nftAmt, "balance of NFT is wrong");
    const [timeRemaining, keepGoing] = makeDeadline(lengthInBlocks);

    var [highestBidder, lastPrice, isFirstBid] = [Auctioneer, minPrice, true];

    invariant(balance(nftId) == nftAmt);
    invariant(balance() == (isFirstBid ? 0 : lastPrice));

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

    commit();
    Owner.publish();

    transfer(nftAmt, nftId).to(highestBidder);

    const auctioneerFee = div(mul(lastPrice, 1), 100);
    const ownerMoney = sub(lastPrice, auctioneerFee);

    if (!isFirstBid) {
        transfer(ownerMoney).to(Owner);
        transfer(auctioneerFee).to(Auctioneer);
    }

    each([Auctioneer, Owner], () => {
        interact.showOutcome(highestBidder, lastPrice);
    });
    commit();
    exit();

});

```git 