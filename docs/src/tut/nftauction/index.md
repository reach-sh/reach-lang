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

In this tutorial, we would go through all the steps for creating a simple decentralized NFT auction application on the algorand blockchain using the REACH framework

This tutorial is for all skill levels, both new and experienced developers in the blockchain space.


## Installation and Configurations

For Windows users, you will need to install wsl and a linux distribution for the wsl (I recommend Ubuntu). You can check out this [link](https://docs.microsoft.com/en-us/windows/wsl/install) for installing a linux distribution with wsl.

You can also check out [link](https://docs.reach.sh/guide/windows/#guide-windows) for a step by step guide for installing reach on windows
 
- You will need to install `make` and also install `Docker `

- To verify if every thing is installed properly run the following commands on your terminal
```bash
make --version
```
```bash
docker --version
```
```bash
docker-compose --version
```
If all this runs succesfully without any errors you are good to go to the next step

- Lets create a directory for this project. I would recommend using 
```bash
mkdir -p ~/reach/nft-timed-auction && cd ~/reach/nft-timed-auction
```
- Next lets install reach using the command below
```bash
curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```
Run ```./reach version``` on your terminal to verify if reach was installed properly

- For the next step make sure your docker is open, then navigate to the directory we just created i.e `reach/nft-timed-auction` then open it on your vs code, once in your vs code open the folder ans create a file namely `index.rsh` once done open an integrated terminal and run the following command to install the images
```bash
./reach update
```
this is done on your first time using reach beacause reach needs those images to function ideally.

Now that we have finally installed all our requirements we can dive into the fun part.

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

```

To check if our backend compiles successfully, `cd` into the root directory of your project from your terminal and run the following command `./reach compile`
If all goes well, you should see the following output

Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
  Verifying when NO participants are honest
Checked 48 theorems; No failures!

Reach hase checked for all possible theorems and has confirmed that no theorem fails. It also confirms that all participants in the application are `honest`

Moving on to our React frontend, Let's start by creating a `index.js` file in the root directory

```js

1 import React from 'react';
2 import Auctioneer from './Components/Auctioneer';
3 import Bidder from './Components/Bidder';
4 import Owner from './Components/Owner';
5 import AppViews from './views/AppViews';
6 import { renderDOM, renderView } from './views/render';
7 import './index.css';

```

- Line 1 to 7 is used to import modules we are yet to define that will be used in the React application


```js
8 import { loadStdlib } from '@reach-sh/stdlib';
9 const reach = loadStdlib(process.env);

11 const { standardUnit } = reach;
12 const defaults = { defaultFundAmt: '1', defaultMinPrice: 1, standardUnit };

```

- Line 8, we import the reach standard library into the React application. It gives us access to the methods, properties in the Reach library
- On Line 9, we assign the `loadStdlib` function to a constant `reach ` passing in the `process.env` object as a parameter to the function
- The `standardUnit` property is imported from the reach standard library. It is used to get the current unit of the network the application is deployed on.
- On Line 12, we define some defaults values we will use globally in the application


Let's define our main class component in the react application

```js
class App extends React.Component {
 constructor(props) {
        super(props);
        this.state = { view: 'Welcome', ...defaults };
    }
19    async createAccount(acc) {
20        const balAtomic = await reach.balanceOf(acc);
21        const bal = reach.formatCurrency(balAtomic, 4);
        this.setState({ acc, bal });
23        if (await reach.canFundFromFaucet()) {
            this.setState({ view: 'FundAccount' });
        } else {
            this.setState({ view: 'OwnerAuctioneerOrBidder' });
        }
    }
29    async createTestAccount() {
30        const acc = await reach.newTestAccount(reach.parseCurrency(1));
        this.createAccount(acc);
    }
33    async typeAccountSecret() {
        this.setState({ view: 'TypeAccountSecret' });
    }
36    async createAccountFromSecret(secret) {
37        const acc = await reach.newAccountFromSecret(secret);
        this.createAccount(acc);
    }
40    async fundAccount(fundAmount) {
41        await reach.fundFromFaucet(this.state.acc, reach.parseCurrency(fundAmount));
        this.setState({ view: 'OwnerAuctioneerOrBidder' });
    }
44    async skipFundAccount() { this.setState({ view: 'OwnerAuctioneerOrBidder' }); }
45    selectOwner() { this.setState({ view: 'Wrapper', ContentView: Owner }); }
46    selectAuctioneer() { this.setState({ view: 'Wrapper', ContentView: Auctioneer }); }
47    selectBidder() { this.setState({ view: 'Wrapper', ContentView: Bidder }); }
48    render() { return renderView(this, AppViews); }
}   

52 renderDOM(<App />);

```

Once the application loads, we set the initial state of the application. The view is set to the `Welcome` view with the defaults values also passed into the state object
- In the `createAuction` function on line 19 we pass in the `acc` (account details) created. 
- Line 20, we check the balance of the current account calling the `balanceOf` method loaded with the standard library class
- The balance is formated to 4 decimal places with the `formatCurrency` method 
- The method defined on line 29, is used to create a test account for the current user with a starting balance defined `reach.newTestAccount(reach.parseCurrency(1));`

- The `typeAccountSecret` and `createAccountFromSecret` methods are used to create an account from an account passphrase provided by the user


- Note: The `view` object key-value pair is used to set the current view of the application.


- The `fundAccount` function on line 40 calls the the `reach.fundFromFaucet` method in its code block. This method allows adding of addtional funds from the faucet into the user's wallet

- The `selectOwner`, `selectAuctioneer`, `SelectBidder` functions on line 45, 46, and 47 respectively sets the current component state to be rendered depending on the selected option on the frontend view

- The `App` component is rendered on the `DOM` on line 52.

Let's create the views for our index.js

In the root directory of the project, create a folder called `views`. Inside the folder, create a new file called `AppViews.js`. The views to be rendered for the `App` component are specified in this file

`AppViews.js`

```js

import React from 'react';

const exports = {};

exports.Wrapper = class extends React.Component {
  render() {
    const { content } = this.props;
    return (
      <div className="App">
        <header className="text-xl App-header" id="root">
          <h1>NFT Auction</h1>
          {content}
        </header>
      </div>
    );
  }
}

exports.Welcome = class extends React.Component {
  render() {
    const { parent } = this.props;
    return (
      <div>
        Welcome to the NFT Auction app!!!
        <br />
        Would you like to create an account?
        <br />
        <button onClick={() => parent.createTestAccount()}>Yes</button>
        <button onClick={() => parent.typeAccountSecret()}>No</button>
      </div>
    );
  }
}

exports.TypeAccountSecret = class extends React.Component {
  render() {
    const secret = (this.state || {}).secret;
    const { parent } = this.props;
    return (
      <div>
        Type account secret
        <br />
        <input
          onChange={(e) => this.setState({ secret: e.currentTarget.value })}
        />
        <br />
        <button onClick={() => parent.createAccountFromSecret(secret)}>Link</button>
      </div>
    );
  }
}

exports.ConnectAccount = class extends React.Component {
  render() {
    return (
      <div>
        Please wait while we connect to your account.
        If this takes more than a few seconds, there may be something wrong.
      </div>
    )
  }
}

exports.FundAccount = class extends React.Component {

  onFundAccount(parent, amt) {
    if (amt < 1 || amt == null) {
      alert('Amount too low, Please try with a higher value (1 and above)');
    } else {
      parent.fundAccount(amt);
    }
  }

  render() {
    const { bal, standardUnit, defaultFundAmt, parent } = this.props;
    const amt = (this.state || {}).amt || defaultFundAmt;
    return (
      <div>
        <h2>Fund account</h2>
        <br />
        Balance: {bal} {standardUnit}
        <hr />
        Would you like to fund your account with additional {standardUnit}?
        <br />
        (This only works on certain devnets)
        <br />
        <input
          type='number'
          placeholder={defaultFundAmt}
          onChange={(e) => this.setState({ amt: e.currentTarget.value })}
        /> micro {standardUnit}
        <button onClick={() => { this.onFundAccount(parent, amt) }}>Fund Account</button>
        <button onClick={() => parent.skipFundAccount()}>Skip</button>
      </div>
    );
  }
}


exports.OwnerAuctioneerOrBidder = class extends React.Component {
  render() {
    const { parent } = this.props;
    return (
      <div>
        Please select a role:
        <br />
        <p>
          <button
            onClick={() => parent.selectOwner()}
          >Owner</button>
          <br /> Set NFT.
        </p>
        <p>
          <button
            onClick={() => parent.selectAuctioneer()}
          >Auctioneer</button>
          <br /> Set the minimum price, deploy the contract.
        </p>
        <p>
          <button
            onClick={() => parent.selectBidder()}
          >Bidder</button>
          <br /> Attach to the Creator's contract.
        </p>
      </div>
    );
  }
}

export default exports;

```

Let's also define the styles to be applied to our HTML elements
Create an `index.css` file in the root directory and paste the following code below

```css

body {
    margin: 0;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Oxygen',
      'Ubuntu', 'Cantarell', 'Fira Sans', 'Droid Sans', 'Helvetica Neue',
      sans-serif;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }
  
  code {
    font-family: source-code-pro, Menlo, Monaco, Consolas, 'Courier New',
      monospace;
  }
  .App {
    text-align: center;
  }
  
  .App-header {
    background-color: #282c34;
    min-height: 100vh;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    font-size: calc(10px + 2vmin);
    color: white;
  }
  
  .App-link {
    color: #61dafb;
  }
  
  input[type=number] {
    font-size: calc(10px + 2vmin);
    width: 4em;
    margin-left: 5px;
    margin-right: 5px;
  }

  input[type=text] {
    font-size: calc(10px + 2vmin);
    width: 4em;
    margin-top: 20px;
    margin-left: 5px;
    margin-right: 5px;
  }
  
  button {
    font-size: calc(10px + 2vmin);
    margin: 4px;
    margin-top: 20px;
  }
  
  textarea {
    font-size: calc(10px + 2vmin);
  }
  
  .ContractInfo {
    font-size: calc(10px + 1vmin);
    text-align: left;
    width: 90vw;
    height: 7em;
    padding: 2vw;
    overflow-x: scroll;
    white-space: pre;
  }


```

Still in the root directory of our project, we are going to create a folder called `Components` to organize the class components defined in our react application. We are going to have three (3) components defined in the folder `Auctioneer.js`, `Bidder.js`, `Owner.js`

`Auctioneer.js` file

```js
import React from 'react';
import AuctioneerViews from '../views/AuctioneerViews';
import { renderView } from '../views/render';
import * as backend from '../build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);
const { standardUnit } = reach;

class Auctioneer extends React.Component {
    constructor(props) {
        super(props);
        this.state = { view: 'Attach' };
    }
14    async attach(ctcInfoStr) {
        const { acc } = this.props;
        const ctc = acc.contract(backend, JSON.parse(ctcInfoStr));
        this.setState({ view: 'Attaching' });
        this.setState({ view: 'SetMinimumPrice', standardUnit, ctc });
    }
20    setMinimumPrice(minPrice) { this.setState({ view: 'SetMinimumBidDiff', minPrice }); }
21    setMinimumBidDiff(minBidDiff) { this.setState({ view: 'SetAuctionLength', minBidDiff }); }
22    setAuctionLength(lengthInBlocks) { 
        this.setState({ view: 'StartingAuction', lengthInBlocks }); 
        backend.Auctioneer(this.state.ctc, this);
    }
26    async startAuction() {
        const { minPrice, minBidDiff, lengthInBlocks } = this.state;
        const auctionParams = { minPrice: Number(minPrice), minBidDiff: Number(minBidDiff), lengthInBlocks };
        this.setState({ view: 'WaitingForBidders' });
        return auctionParams;
    }
32    seeBid(who, amt) {
        { this.setState({ view: 'SeeBid', standardUnit, who: reach.formatAddress(who), amt: reach.formatCurrency(amt) }); }
    }
35    showOutcome(winner, amt) {
        this.setState({ view: 'ShowOutcome', standardUnit, winner: reach.formatAddress(winner), amt: reach.formatCurrency(amt) });
    }
38    restart() {
        this.setState(null);
        this.setState({ view: 'Attach' });
    }

43    render() { return renderView(this, AuctioneerViews); }
}

export default Auctioneer;
```

In the `Auctioneer.js` component file we mirror the `startAuction`, `seeBid`, `showOutcome` functions defined in the Auctioneer Participant interface in the backend

We have frontend specific functions defined: `setMinimumPrice` used to pass the value of the minimum bid price specified by the Auctioneer participant to the state object. `setMinimumBidDiff` to get the `minBidDiff` required for the auction to the state object and `setAuctionLength` to get the length in blocks `lengthInBlocks` of the auction.

The attach function is used to 'attach' the Auctioneer participant to the contract that will be deployed by the Owner participant.

The startAuction fuction is called immediately, once the `Auctioneer` joins the contract. The `auctionParams` containing the `minPrice`, `minBidDiff`, and `lengthInBlocks` is returned from the function call

```js

    async startAuction() {
        const { minPrice, minBidDiff, lengthInBlocks } = this.state;
        const auctionParams = { minPrice: Number(minPrice), minBidDiff: Number(minBidDiff), lengthInBlocks };
        this.setState({ view: 'WaitingForBidders' });
        return auctionParams;
    }

```

- On Line 38, we define a function that is used to reset the state of the application on call once the auction is over. It defaults the react application back to the `Attach` view for an Auctioneer Participant.
- The `AuctioneerViews` are rendered on Line 43 

AttacherViews.js (views/AttacherViews.js)

```js
import React from 'react';
import BidderViews from './BidderViews';

const exports = { ...BidderViews };

exports.Wrapper = class extends React.Component {
  render() {
    const { content } = this.props;
    return (
      <div className="Attacher">
        <h2>Bidder</h2>
        {content}
      </div>
    );
  }
}

exports.Attach = class extends React.Component {

  onAttach(parent, ctcInfoStr, nftId) {
    if (!ctcInfoStr || !nftId) {
      alert('Please insert required information')
    } else {
      parent.attach(ctcInfoStr, nftId)
    }
  }
  render() {
    const { parent } = this.props;
    const { ctcInfoStr, nftId } = this.state || {};
    return (
      <div>
        Please paste the contract info to attach to:
        <br />
        <textarea spellCheck="false"
          className='ContractInfo'
          onChange={(e) => this.setState({ ctcInfoStr: e.currentTarget.value })}
          placeholder=''
        />
        <br />
        Please paste the NFT id to accept:
        <br />
        <textarea spellCheck="false"
          className='ContractInfo'
          onChange={(e) => this.setState({ nftId: e.currentTarget.value })}
          placeholder=''
        />
        <br />
        <button
          disabled={!ctcInfoStr || !nftId}
          onClick={() => {this.onAttach(parent, ctcInfoStr, nftId)}}
        >Attach</button>
        <br />
      </div>
    );
  }
}

exports.Attaching = class extends React.Component {
  render() {
    return (
      <div>
        Attaching, please wait...
      </div>
    );
  }
}


export default exports;

```

AuctioneerViews.js (views/AuctioneerViews.js)

```js
import React from 'react';

const exports = {};

exports.Wrapper = class extends React.Component {
  render() {
    const { content } = this.props;
    return (
      <div className="Auctioneer">
        <h2>Auctioneer</h2>
        {content}
      </div>
    );
  }
}

exports.Attach = class extends React.Component {

  onAttach(parent, ctcInfoStr) {
    if (!ctcInfoStr) {
      alert('Please insert required information')
    } else {
      parent.attach(ctcInfoStr)
    }
  }
  render() {
    const { parent } = this.props;
    const { ctcInfoStr } = this.state || {};
    return (
      <div>
        Please paste the contract info to attach to:
        <br />
        <textarea spellCheck="false"
          className='ContractInfo'
          onChange={(e) => this.setState({ ctcInfoStr: e.currentTarget.value })}
          placeholder=''
        />
        <br />
        <button
          disabled={!ctcInfoStr}
          onClick={() => { this.onAttach(parent, ctcInfoStr) }}
        >Attach</button>
        <br />
      </div>
    );
  }
}

exports.Attaching = class extends React.Component {
  render() {
    return (
      <div>
        Attaching, please wait...
      </div>
    );
  }
}

exports.SetMinimumPrice = class extends React.Component {

  onSetMinimumPrice(parent, minPrice) {
    if (minPrice < 1 || minPrice == null) {
      alert('Minimum price too low, Please try with a higher value (1 and above)');
    } else {
      parent.setMinimumPrice(minPrice);
    }
  }
  render() {
    const { parent, defaultMinPrice, standardUnit } = this.props;
    const minPrice = (this.state || {}).minPrice || defaultMinPrice;
    return (
      <div>
        <input
          required
          type='number'
          placeholder={defaultMinPrice}
          onChange={(e) => this.setState({ minPrice: e.currentTarget.value })}
        /> micro {standardUnit}
        <br />
        <button
          onClick={() => { this.onSetMinimumPrice(parent, minPrice) }}
        >Set minimum price</button>
      </div>
    );
  }
}

exports.SetMinimumBidDiff = class extends React.Component {

  onSetMinimumBidDiff(parent, minBidDiff) {
    if (minBidDiff < 1 || minBidDiff == null) {
      alert('Minimum bid value too low, Please try with a higher value (1 and above)');
    } else {
      parent.setMinimumBidDiff(minBidDiff);
    }
  }
  render() {
    const { parent, defaultMinBidDiff, standardUnit } = this.props;
    const minBidDiff = (this.state || {}).minBidDiff || defaultMinBidDiff;
    return (
      <div>
        <input
          required
          type='number'
          placeholder={defaultMinBidDiff}
          onChange={(e) => this.setState({ minBidDiff: e.currentTarget.value })}
        /> micro {standardUnit}
        <br />
        <button
          onClick={() => { this.onSetMinimumBidDiff(parent, minBidDiff) }}
        >Set minimum bid difference</button>
      </div>
    );
  }
}


exports.SetAuctionLength = class extends React.Component {
  onSetAuctionLength(parent, lengthInBlocks) {
    if (lengthInBlocks < 1 || lengthInBlocks == null) {
      alert('Length too low, Please try with a higher value (1 and above)');
    } else {
      parent.setAuctionLength(lengthInBlocks);
    }
  }
  render() {
    const { parent } = this.props;
    const lengthInBlocks = (this.state || {}).lengthInBlocks
    return (
      <div>
        Set Auction length in blocks
        <br />
        <input
          type='number'
          onChange={(e) => this.setState({ lengthInBlocks: e.currentTarget.value })}
        />
        <br />
        <button
          onClick={() => { this.onSetAuctionLength(parent, lengthInBlocks) }}
        >Save</button>
      </div>
    );
  }
}

exports.StartingAuction = class extends React.Component {
  render() {
    return (
      <div>
        Starting Auction...
      </div>
    );
  }
}


exports.WaitingForBidders = class extends React.Component {

  render() {
    return (
      <div>
        Waiting for Bidders to join...
      </div>
    )
  }
}

exports.SeeBid = class extends React.Component {
  render() {
      const { who, amt, standardUnit } = this.props;
      return (
          <div>
              Bidding in progress
              <br />
              {who} decided to bid {amt} {standardUnit}
          </div>
      );
  }
}


exports.ShowOutcome = class extends React.Component {
  render() {
      const { winner, amt, standardUnit, parent } = this.props;
      return (
          <div>
              Auction has ended
              <br />
              {winner} won with a bid of {amt} {standardUnit}
              <br />
              <button
                  onClick={(e) => parent.restart()}
              >Restart</button>
          </div>
      );
  }
}

export default exports;
```


Building our `Owner.js` component

`Owner.js` file

```js
import React from 'react';
import OwnerViews from '../views/OwnerViews';
import { renderView } from '../views/render';
import * as backend from '../build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);
const { standardUnit } = reach;

class Owner extends React.Component {
    constructor(props) {
        super(props);
        this.state = { view: 'SetNFT' };
    }
    setMyNFT(nftId) { this.setState({ view: 'Deploy', nftId }); }
    async setDemoNFT() {
        const NFT = await reach.launchToken(this.props.acc, "Omz", "NFT", { supply: 1 });
        this.setState({ view: 'Deploy', nftId: NFT.id })
    }
    async setNFT() {
        const nftId = this.state.nftId;
        return { nftId };
    }
    async deploy() {
        const ctc = this.props.acc.contract(backend);
        this.setState({ view: 'Deploying', ctc });
        backend.Owner(ctc, this);
        const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
        const nftId = JSON.stringify(this.state.nftId);
        this.setState({ view: 'WaitingForAuctionToStart', ctcInfoStr, nftId });
    }
    seeBid(who, amt) {
        { this.setState({ view: 'SeeBid', standardUnit, who: reach.formatAddress(who), amt: reach.formatCurrency(amt) }); }
    }
    async showOutcome(winner, amt) {
        const balance = await this.getBalance();
        const amtNFT = await this.getNFTBalance(this.state.nftId);
        this.setState({ view: 'ShowOutcome', standardUnit, winner: reach.formatAddress(winner), amt: reach.formatCurrency(amt), balance, amtNFT });
    }
    async getBalance() {
        return reach.formatCurrency(await reach.balanceOf(this.props.acc));
    }
    async getNFTBalance(nftId) {
        return await this.props.acc.balanceOf(JSON.parse(nftId));
    }
    restart() {
        this.setState(null);
        this.setState({ view: 'SetNFT' });
    }

    render() { return renderView(this, OwnerViews); }
}

export default Owner;

```
Similar to the Auctioneer component, we have the `seeBid`, `showOutcome` and `restart` functions. In addition to these functions, the Owner component has the following unique functions `setMyNFT` for passing in the `nftId` to the state object, `setDemoNFT` for creating a demo NFT that will used for the auction, `setNFT` a function defined in the Owner Participant interface that will be called once the contract is deployed. The logic for deploying the smart contract from the frontend is defined in the `deploy` function. 
- The `backend.owner` expression in the deploy function, ensures the participant is deployed on the smart contract as an Owner participant

```js
    async deploy() {
        const ctc = this.props.acc.contract(backend);
        this.setState({ view: 'Deploying', ctc });
        backend.Owner(ctc, this);
        const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
        const nftId = JSON.stringify(this.state.nftId);
        this.setState({ view: 'WaitingForAuctionToStart', ctcInfoStr, nftId });
    }
```

We also have functions to get the current balance and NFT balance of the current account in the contract `getBalance` and `getNFTBalance` respectively.

OwnerViews.js  (views/OwnerViews.js)

```js
import React from 'react';

const exports = {};

const sleep = (milliseconds) => new Promise(resolve => setTimeout(resolve, milliseconds));

exports.Wrapper = class extends React.Component {
    render() {
        const { content } = this.props;
        return (
            <div className="Owner">
                <h2>Owner</h2>
                {content}
            </div>
        );
    }
}

exports.SetNFT = class extends React.Component {
    render() {
        const { parent } = this.props;
        const nftId = (this.state || {}).nftId
        return (
            <div>
                Set nft id to auction
                <br />
                <input
                    type='text'
                    onChange={(e) => this.setState({ nftId: e.currentTarget.value })}
                />
                <br />
                <button
                    onClick={() => parent.setMyNFT(nftId)}
                >Save</button>
                <br />
                OR
                <br />
                <button onClick={() => parent.setDemoNFT()}>Use demo NFT</button>
            </div>
        );
    }
}

exports.Deploy = class extends React.Component {
    render() {
        const { parent } = this.props;
        return (
            <div>
                <br />
                <button
                    onClick={() => parent.deploy()}
                >Deploy</button>
            </div>
        );
    }
}

exports.Deploying = class extends React.Component {
    render() {
        return (
            <div>Deploying... please wait.</div>
        );
    }
}


exports.WaitingForAuctionToStart = class extends React.Component {

    async copyToClipboard(button, value) {
        navigator.clipboard.writeText(value);
        const origInnerHTML = button.innerHTML;
        button.innerHTML = 'Copied!';
        button.disabled = true;
        await sleep(1000);
        button.innerHTML = origInnerHTML;
        button.disabled = false;
    }
    async copyContractInfoToClipboard(button) {
        const { ctcInfoStr } = this.props;
        this.copyToClipboard(button, ctcInfoStr);
    }

    async copyNFTIdToClipboard(button) {
        const { nftId } = this.props;
        this.copyToClipboard(button, nftId);
    }

    render() {
        const { ctcInfoStr, nftId } = this.props;
        return (
            <div>
                Waiting for Bidders to join...
                <br /> Please give them this contract info:
                <pre className=''>
                    {ctcInfoStr}
                </pre>
                <button
                    onClick={(e) => this.copyContractInfoToClipboard(e.currentTarget)}
                >Copy to clipboard</button>
                <br />
                <br />
                And also the NFT id
                <pre className=''>
                    {nftId}
                </pre>
                <button
                    onClick={(e) => this.copyNFTIdToClipboard(e.currentTarget)}
                >Copy to clipboard</button>
                <br />

            </div>
        )
    }
}

exports.SeeBid = class extends React.Component {
    render() {
        const { who, amt, standardUnit } = this.props;
        return (
            <div>
                Bidding in progress
                <br />
                {who} decided to bid {amt} {standardUnit}
            </div>
        );
    }
}


exports.ShowOutcome = class extends React.Component {
    render() {
        const { winner, amt, standardUnit, parent, balance, amtNFT } = this.props;
        return (
            <div>
                Auction has ended
                <br />
                {winner} won with a bid of {amt} {standardUnit}
                <br />
                {`You have ${balance} ${standardUnit} and ${amtNFT} NFT`}
                <br />
                <button
                    onClick={(e) => parent.restart()}
                >Restart</button>
            </div>
        );
    }
}

export default exports;

```

Moving to the `Bidder.js` component (Components/Bidder.js)

```js
import React from 'react';
import AttacherViews from '../views/AttacherViews';
import { renderView } from '../views/render';
import * as backend from '../build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);
const { standardUnit } = reach;

class Bidder extends React.Component {
    constructor(props) {
        super(props);
        this.state = { view: 'Attach' };
    }
    async attach(ctcInfoStr, nftId) {
        const { acc } = this.props;
        const jsonNFTId = JSON.parse(nftId);
        await acc.tokenAccept(jsonNFTId);
        this.setState({nftId: jsonNFTId});
        const ctc = acc.contract(backend, JSON.parse(ctcInfoStr));
        this.setState({ view: 'Attaching' });
        this.setState({ view: 'PlaceBid', standardUnit, ctc })
    }
    async placeBid(ctc, bid) {
        let previousBalance = 0;
        let latestBalance = 0;
        try {
            previousBalance = await this.getBalance();
            const [lastBidder, lastBid] = await ctc.apis.Bidder.bid(bid);
            latestBalance = await this.getBalance();
            this.setState(
                {
                    error: null,
                    view: 'PlaceBid',
                    standardUnit,
                    ctc,
                    lastBid: reach.formatCurrency(lastBid),
                    bid: reach.formatCurrency(bid),
                    previousBalance,
                    latestBalance,
                    lastBidder: reach.formatAddress(lastBidder)
                })
        } catch (error) {
            console.log(error);
            if (error.message.includes("is too low")) {
                this.setState(
                    {
                        error,
                        view: 'PlaceBid',
                        standardUnit,
                        ctc,
                        previousBalance,
                        latestBalance
                    })
            } else {
                const amt = await this.getBalance();
                const amtNFT = await this.getNFTBalance(this.state.nftId);
                this.setState({ view: 'Error', amt, standardUnit, amtNFT });
            }

        }
    }
    async getBalance() {
        return reach.formatCurrency(await reach.balanceOf(this.props.acc));
    }
    async getNFTBalance(nftId) {
        return await this.props.acc.balanceOf(nftId);
    }
    render() { return renderView(this, AttacherViews); }
}

export default Bidder;

```
We have similar functions defined in both the Auctioneer, and Owner component. The unique function in this component will be the `placeBid` function. The `placeBid` function receives the `bid` from a form in the view and uses it to call the `bid` function defined in the API interface `Bidder` in the backend. The `bid` function call
`const [lastBidder, lastBid] = await ctc.apis.Bidder.bid(bid);` returns the lastBidder, and lastBid properties. It is placed in a try-catch block, to catch for any errors that may occur while placing a bid.
- In the catch block, we check if the error message contains `is too low`. A bid is too low message, or bid difference is too low message if any of the checks defined in the fork.api_ statement fails.
- Any other error, will signify an ended auction

BidderViews.js (Views/BidderViews.js)

```js
import React from 'react';

const exports = {};

// Player views must be extended.
// It does not have its own Wrapper view.

exports.PlaceBid = class extends React.Component {
  render() {
    const {
      error,
      standardUnit,
      ctc,
      parent,
      lastBid,
      bid,
      previousBalance,
      latestBalance,
      lastBidder
    } = this.props;
    const bidPlaced = (this.state || {}).bidPlaced;
    return (
      <div>
        Balance before: {previousBalance ? previousBalance : ''} {standardUnit}
        <br />
        Balance after: {latestBalance ? latestBalance : ''} {standardUnit}
        <br />
        Please place your bid
        <br />
        {lastBid ? `You out bid the last bid of ${lastBid} ${standardUnit}` : ''}
        <br />
        {lastBid ? `from ${lastBidder}` : ''}
        <br />
        {bid ? `With a bid of ${bid} ${standardUnit}` : ''}
        <br />
        <input
          required
          type='number'
          onChange={(e) => this.setState({ bidPlaced: e.currentTarget.value })}
        /> micro {standardUnit}
        <br />
        {error ? 'Bid or bid difference is too low, please try again.' : ''}
        <br />
        <button
          onClick={() => parent.placeBid(ctc, bidPlaced)}
        >Place bid</button>
      </div>
    );
  }
}

exports.Done = class extends React.Component {
  render() {
    const { outcome } = this.props;
    return (
      <div>
        Thank you for playing. The outcome of this game was:
        <br />{outcome || 'Unknown'}
      </div>
    );
  }
}

exports.Error = class extends React.Component {
  render() {
    const { amt, standardUnit, amtNFT } = this.props;
    return (
      <div>
        You failed to bid, because the auction is over
        <br/>
        {`You have ${amt} ${standardUnit} and ${amtNFT} NFT`}
      </div>
    );
  }
}

export default exports;

```

All page rendering through the current 'view' state of the application is made possible through the logic defined in `render.js`

render.js  (views/render.js)

```js
import ReactDOM from 'react-dom';
import React from 'react';

export function renderDOM(app) {
  ReactDOM.render(
    <React.StrictMode>{app}</React.StrictMode>,
    document.getElementById('root')
  );
}

export function renderView(parent, Views) {
  parent.state = parent.state || {};
  const {view, ContentView} = parent.state;
  const View = view === 'Wrapper'
    ? ContentView
    : Views[view];
  const Wrapper = Views['Wrapper'];
  const props = {...parent.props, ...parent.state, parent};
  const content = <View {...props} />;
  return <Wrapper {...{content}} />;
}
```

# Conclusion 

Congratulations!!!  We have come to the end of this tutorial - NFT Timed Auction with React frontend.

You should have the following code structure

Components
    Auctioneer.js
    Bidder.js
    Owner.js
views
    AppViews.js
    AttacherViews.js
    AuctioneerViews.js
    BidderViews.js
    OwnerViews.js
    render.js
index.css
index.js
index.rsh

Once you have verified that all files are existing with the right code, you can run your application with the newly created frontend using the following command from the root directory of your project in a  terminal window 
`REACH_CONNECTOR_MODE=ALGO-devnet ./reach react`

The `REACH_CONNECTOR_MODE` defines the network the application should run on, iny my case, I selected the 'ALGO-devnet' network.

After succesful compilation, you can view your application on `localhost:3000`

For the link to the full repo check out the [link](https://github.com/Omzlaw/reach-auction).