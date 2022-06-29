# {#nft} Build an NFT Auction with Reach

Alice and Bob are now ready to kick-off development of their Algorand-powered auction DApp! 
This guide will take you through the steps to build the DApp, 
which includes setting up a development environment, 
writing the backend and frontend, 
and deploying it.

The goal of this guide is to get you up and running with a working prototype that represents a real use case. 
Although, we use sound design principles and best practices for building DApps with Reach, 
this guide does not cover every detail. 
This is intentional so you can focus on foundational concepts for building any DApp on Algorand using Reach. 
Don’t worry if you don’t understand what everything does in the solution. 
This is expected! 

After you feel comfortable with the basics, 
you can head over to the detailed Reach [tutorials](https://docs.reach.sh/tut/#tuts) and become an expert in the Reach language.

All of the code for this guide is [open source](https://github.com/reach-sh/reach-lang/tree/master/examples/nft-auction-api). 
To get the most out of this guide, 
we recommend reproducing the code in a [text editor](https://code.visualstudio.com/Download). 

When learning a new language, 
it is a best practice to type the code yourself and flip between windows, 
rather than splitting the screen. 
While this process is not as efficient, 
it is much more effective in forcing you to think about the code as you recreate it.

# Organization

This guide is organized into four sections: 
1. A summary of [Reach Architecture](#reach-architecture)
1. An [install guide](#install-reach) to get up and running with Reach. 
1. An [overview](#application-overview) of the Reach DApp.
1. A step by step guide and explanation of components used while [building](#building-a-dapp) a Reach application.

# Reach Architecture

Using a high-level language such as Reach to build DApps instead of low-level assembly language is attractive for a number of reasons. 
First, abstraction is a developer's friend. 
Abstraction allows developers to focus on what really matters in their application, 
such as, codifying the business logic of their program, 
rather than determining where the application should access memory.

Another reason is that most humans, even most engineers, 
are not good at writing low-level code that is efficient, secure, and free of flaws. 
Reach abstracts contract storage, protocol diagrams, state validation and network details so you can focus on how participants, users of the DApp, interact with one another and the contract.

Reach's backend is very often one single Reach `rsh` file, which is syntactically the same as a JavaScript file, 
but contains web3 functionality.
The frontend in a Reach DApp can be created with JavaScript using Reach's [Standard JavaScript Library](https://docs.reach.sh/frontend/#ref-frontends).
Alternatively, developers can use Python, Go, or C# in their frontend by utilizing Reach's [RPC Server](https://docs.reach.sh/rpc/).
Or they can directly use the Algorand SDK and interact with their application using the standard Algorand ABI.

Reach's backend, often named `index.rsh`, 
orchestrates the data to be published to the consensus network (a blockchain) and defines interfaces that can interact with the frontend. 
This interface is referred to as a Participant Interact Interface. 
The frontend defines the interact objects and is often contained within a single file called `index.mjs`. 
The frontend provides an interface to the participants, e.g. Alice and Bob. 
The user interface may be in the form of a command line, 
or more preferably, a web or mobile app.

In Reach, developers think about their participants and how they will interact with one another and the contract. 
Developers codify the business logic of their application and the Reach compiler assembles the bytecode and smart contract. 
Reach abstracts the heavy lifting of smart contract development, 
allowing developers to focus their energy on participant interaction. 

Additionally, Reach's built-in formal verification engine ensures that the contract has the ability to exit, 
protects against common blockchain attacks, 
guarantees the contract exits with a zero balance (thus preventing locked away and inaccessible tokens),
verifies the contract won't overspend, 
confirms that formulas are true whether participants and frontends are honest or dishonest, 
and guarantees token linearity. 

As a result, auditing Reach DApps is easier and less expensive when compared to low-level alternative programming languages. 
Reach's compiler is more efficient than most human-built low-level code. 
Altogether, Reach is the smartest, fastest, and safest way to build web3 decentralized applications.

## Modes of Reach

Understanding the modes of a Reach DApp is helpful when building with Reach and will improve your comprehension when working through this guide.

Reach backends are comprised of four primary modes: 
* App Initialization
* Local Step
* Step
* Consensus Step

App Initialization refers to the section beginning at the creation of the Participant Interact Interface and ending at the `init()` statement. 

A local step occurs on a participant's local machine. 
Information discovered in the local step remains private until it is declassified and published in a consensus step.

Step refers to the state of the application. 
Moving to a new step in the Reach DApp is to enter a new state. 

Consensus Steps are public steps that are entered into transactions in the consensus network (blockchain). Actions executed in a consensus step are immutable and public knowledge.

# Install Reach

Reach is designed to work on POSIX systems with [make](https://en.wikipedia.org/wiki/Make_(software)), [Docker](https://www.docker.com/get-started), and [Docker Compose](https://docs.docker.com/compose/install/) installed. The best way to install Docker on Mac and Windows is with [Docker Desktop](https://www.docker.com/products/docker-desktop). 

[Windows users](https://docs.reach.sh/guide/windows/#guide-windows) need to [install WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10), a Windows subsystem for Linux, and run Reach through an Ubuntu command-line as administrator. 

To confirm dependencies are installed, try to successfully run the following three terminal commands.

```
  $ make --version
  $ docker --version
  $ docker-compose --version
```

If you’re using Windows, consult [the guide to using Reach on Windows](https://docs.reach.sh/guide-windows.html).

Next, make a directory for Reach and a subdirectory for the Auction DApp:

``` bash
  $ mkdir -p ~/Reach/api-nft-auction && cd ~/Reach/api-nft-auction
```

If your terminal has trouble with this command, create the directories one at a time:

``` bash
  $ mkdir Reach
  $ cd Reach
  $ mkdir api-nft-auction
  $ cd api-nft-auction
```

Create two files: The backend Reach file: `index.rsh` and the frontend JavaScript file: `index.mjs`.

``` bash
  $ touch index.rsh
  $ touch index.mjs
```

Next, download Reach with the following curl command:

``` bash
  $ curl https://docs.reach.sh/reach -o reach ; chmod +x reach
```

Since Reach is Dockerized, the necessary images are downloaded automatically when used for the first time, 
but can be done manually by running

```bash
  $ ./reach update
```

## Setup environments

Set an environment variable to use the Algorand connector. 

``` bash
$ export REACH_CONNECTOR_MODE=ALGO
```

# Application Overview

This Decentralized Auction involves a Creator who establishes the sale and Buyers who bid on an NFT in ALGOs. 
If the bid is successful, the DApp will hold the ALGOs. 
Bidding is only allowed while the auction is active. 
If the bid supplants a previous bid, then the contract automatically refunds the previous higher bidder’s ALGOs. 
At the end of the auction, the highest bidder receives the NFT and the contract transfers the funds to the Creator.

<center>
![Auction close](../../imgs/dapp-close.png){: style="width:500px" align=center }
<figcaption style="font-size:12px">Close out the auction: Carla receives the NFT and Alice gets paid 2000 ALGOs.</figcaption>
</center>

# Building a DApp

It is beneficial to take time to think about the DApp before beginning to code. 
Follow these steps to efficiently develop a Reach DApp:

1. [Evaluate the problem](#problem-analysis) to solve. 
1. Itemize the [data definitions](#data-definition) and their primitive types.
1. Make a list of the participants and how they will [interact](#communication-construction) with one another and the contract. 
1. Write all [assumptions](#assertion-insertion) about the business logic of the DApp.

Once this information is clarified a flow chart can be created to dictate the organization of the DApp.

## Problem Analysis

Let's begin by defining the problem that this DApp will solve:

1. Collectors of NFTs desire a secondary marketplace to sell their assets, 
and buyers desire to purchase NFTs that were not previously available.
1. Sellers must be able to create a new auction for each piece of artwork. 
1. The NFT must be held by the contract after the auction begins and until the auction closes.
1. For each bid, the new bid must be higher than the previous bid. 
1. The previous bid will be refunded to the previous bidder and the new bid will be recorded as the highest bid and be held by the contract.
1. At the end of a successful auction the highest bidder will receive the NFT and the seller will receive the full bid amount.

## Data Definition

The necessary data points and their types can be determined now that the problem has been analyzed.

In Reach, global constants can be defined using the `const` keyword. 
Localized backend methods are defined in the Participant Interact Interface, which dictates the DApp's communication model. 
The methods in each participant's interact interface are reflected as Interact Objects in the frontend. 

Let's define the methods in the Participant Interact Interface:

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 1-17
```

This block initializes the DApp and populates the Participant Interact Interface.

+ Line 1: This is the first line in every Reach program and tells the compiler what version of Reach to use
+ Line 3: Initializes the Reach Application
+ Line 4: Instantiates the `Participant` "Creator"
+ Lines 5-9: Defines `getSale`, a function that has no input and outputs an object with three key-value pairs. 
`nftId` is a `Token`, and `minBid` and `lenInBlocks` are both unsigned integers.
+ Line 10: Creates `auctionReady`, a function that has no inputs or outputs
+ Line 11: Realizes `seeBid`, a function that takes an `Address` and `UInt` as inputs and has no outputs
+ Line 12: The final function, `showOutcome` which also takes an `Address` and `UInt` as inputs and has no output.
+ Line 14: Defines an `API` named `"Bidder"`
+ Line 15: Names `Bidder`'s only function, `bid`, which takes an unsigned integer as an input and outputs a `Tuple` containing an `Address` and `UInt`.
+ Line 17: Initializes the application and begins the first step of the Reach DApp.

Now, let's set up the initial data definitions in the frontend.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 1-15
```

+ Lines 1-2: Imports Reach's standard JavaScript library and the compiled version of `index.rsh`
+ Lines 4-5: Defines the standard library as `stdlib` and creates `devnet` tokens
+ Lines 7-8: Creates a test account for the Creator and supplies it with the `devnet` tokens
+ Lines 10-14: Creates the NFT for testing the DApp
Notice that `nftId`, `minBid`, and `lenInBlocks` from the backend are all given values
+ Line 15: The NFT parameters are stored in an object

Before returning to the backend, let's define the Creator's interact object. 
This object will be created after the `Bidder`'s `API` logic, which is covered in the next section.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 51-66
```

+ Line 51: The Creator account deploys the Reach application 
+ Line 52: Instantiates the Creator's interact object. 
The interact object includes all of the code up to line 66 
+ Lines 53-56: Defines the functionality of the `getSale` method
+ Lines 57-59: Writes the functionality for the `auctionReady` function
+ Lines 60-62: Defines the logic for `seeBid`
+ Lines 63-65: Defines `showOutcome`

At this point, by comparing the Participant Interact Interface in the backend with the Interact Objects in the frontend it's apparent that the backend functions reflect the frontend functionality. 
The frontend provides the instructions for how each function should behave. 
The backend dictates when each function should be called and how the participants will interact with one another. 
Functions defined in the interact interface will always mirror the functions' logic in the interact object.

The initial data points have been defined. 
Next, we'll turn our attention to the DApp's communication model.

## Communication Construction

The communication model controls how participants interact with one another and the contract. 
Fortunately, we have a roadmap for the communication model because the problem analysis informs its development. 

Per the analysis, the Creator, a `Participant`, submits an NFT to the contract. 
`Bidder`s, who are `API Participants`, begin to bid on the newly posted NFT. 
The highest `bid` is held by the contract. 
New bidders must make a higher `bid` than the previous bidder. 
As new bids are submitted, the contract returns tokens to the outbid `API Participants`. 
Finally, at the end of the auction a winner is announced and the highest `bid` amount is transferred to the Creator in exchange for the NFT.

Let's explore what we mean by `Participants` and `API Participants`.

### Participants

A participant is a user who takes part in the DApp and can have persistently stored values, called its local state. 
Participants are associated with an account (address) on the consensus network. 
A Consensus Network is a Network protocol (a blockchain) that contains network tokens (ALGO, ETH, etc.), 
non-network tokens (ASAs, ERC-20, etc.), 
as well as a set of accounts and contracts.

### API Participants

`API Participants` refer to a class of participants who share the same functionality. 
They are essentially the same type of user who share the same functionality. 

To illustrate, imagine an application where users vote for their favorite color. 
There can be unlimited voters who participate in the act of voting. 
Although each voter is an independent user, they all share the same functionality. 
In this example, all voters are members of the “voter API”. 
Similarly, in this DApp, we have an `API` of `Bidder`s. 
Every bidder within this `API` has the ability to place a `bid`, 
but only one can win the auction.

An `API` is unique because it represents the frontend pinging the backend only as needed.
Using `API`s allows Reach developers to create quieter and more efficient programs that are not as computationally expensive. 

However, at least one user must always be a `Participant` because `API`s are not able to deploy a Reach application. 

### Codify the Communication Model

Now, we can codify the body of the DApp, given our understanding of the communication model, `Participants` and `API`s.

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 19-26
```

+ Line 19: Begins the Creator's local step
+ Line 20: Calls and declassifies the `getSale()` function and stores the output of `getSale()` into an object
+ Line 21: Closes Creator's local step and moves the DApp to the next step of the DApp.
+ Line 22: Moves the DApp into a consensus step and publishes the output of `getSale()` to the blockchain.
+ Line 23: Establishes the amount, `amt`, as "1"
+ Line 24: Concludes the consensus step and returns the DApp to a new step
+ Line 25: Pays the NFT to the contract; essentially, acting as an escrow service provider
+ Line 26: Creator executes the `auctionReady()` method, which initializes `startBidders()`. 
The `interact` method communicates with the frontend to execute the function having the same name.

### Pay and Publish

`pay` and `publish` are common consensus network methods used to write information to the blockchain.  

`pay` allows participants and APIs to `pay` the contract and each other. 

When paying a previously known amount, use `pay`. 
A `pay` transfer always explicitly names the acting participant.

Publish writes information to the blockchain. 
If a participant is sharing information, then use a `publish`. 

In the following example, `publish` is writing the nft ID, reserve price, 
and length in blocks to the consensus network.

```
Creator.publish(nftId, reservePrice, lenInBlocks);
```

In this next example, the creator uses `pay` to transfer a specific NFT to the contract. Later, the contract will `transfer` this NFT to the winning bidder of the auction.

```
Creator.pay([[amt, nftId]]);
```

### ParallelReduce

At this point, we have satisfied the requirement to create `Bidder`s and we allow a `Creator` to submit an NFT to the contract. 
Next, we need to enable `Bidder`s to place bids against one another. 

We could create a `while` loop with `fork` statements based on `case`s where bidders either become the highest bidder or must bid again. 
However, Reach will do the heavy lifting of managing `fork`s and updating `API` values with [parallelReduce](https://docs.reach.sh/rsh/consensus/#parallelreduce). 
A `parallelReduce` initiates a race between bidders, which enables the DApp to update the highest bidder in a consensus step until the auction ends.

:::note
The `parallelReduce` name comes from the fact that participants are trying to produce a new state from the current values in parallel. 
Parallel reduce is essentially a fork+case loop. 
So while `Alice` is producing a new state, `Bob` might also be producing one. 
Reach determines who tried to update the state first (`fork`) and depending on the winner of the race runs a specified logic (`case`).
:::

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 28-37
```

+ Line 28: Defines an expression that determines when the auction will end
+ Line 29-33: Creates the `parallelReduce`; aligns the `Creator` as the first `highestBidder`, the `minBid` as the `lastPrice`, 
and sets `isFirstBid` to the boolean, "true"
+ Line 34: States the first invariant, which asserts that the balance of the `nftID` will be equal to the amount of the NFTs
+ Line 35: Is the second invariant which states that the `balance` will either be 0 if it is the first `bid` or equal to the most recent price
+ Line 36: Initializes the `while` loop and sets the condition to be true until the value in `end` is greater than the value stored in `lastConsensusTime()` 
+ Line 37: Calls the `Bidder API`

This section of the program introduces critical components of a Reach DApp. 
Let's unpack the `lastConsensusTime`, `invariant`, and `api_` methods before moving forward. 

### lastConsensusTime

The `lastConsensusTime` primitive returns the network time of the last publication of the dApp. 
This may not be available if there are no previous publications. 
For this reason, it is a best practice to publish `lastConsensusTime` with the first publication. 

### Invariant

An invariant states properties that will not change before or after a `while` loop. 
Expressions within an invariant must always be true. 
Invariants are true before the contract enters the `while` loop, while in the loop, and after exiting the loop. 
Loop invariants only need to mention values that could vary due to loop executions. 
Learn more about loop invariants in the [Reach Guides](https://docs.reach.sh/guide/loop-invs/#guide-loop-invs).

### API Macro

`api_`, pronounced, "API Macro," is a special macro of `api`. 
The `api_` form can be used when the dynamic assertions, `assume` and `require`, are identical. 
One advantage of `api_` is that it allows Reach developers to write less code, thereby reducing opportunities to introduce bugs. 
It's also a quick indication that the dynamic assertions will not change between local and consensus steps.

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 39-43
```

We'll skip line 38 for the time being and return to it with an explanation in the [Assertion Insertion](#assertion-insertion) section.

+ Line 39-40: Returns the `bid` and a created "notify" argument to update other APIs with the newest high bidder and their price. 
+ Line 41-43: If this `bid` is not the first `bid` then the `api_` will return the `lastPrice` to the address of the previous highest bidder.

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 44-48
```

+ Line 44: Assigns `this` to the constant `who`
+ Line 45: Creator `interact`s with the `seeBid` function, which accepts `who`, an `API` `Bidder`, and their `bid`.
+ Line 46: Returns `highestBidder`, `lastPrice`, and `isFirstBid`. 
The `parallelReduce` will now update the variables in the race. 
As a result, new `Bidder`s will see the updated `highestBidder` and the `lastPrice`.
+ Line 47: Closes the return statement
+ Line 48: Closes the `api_` macro begun in line 37

### Timeout

Next, a `timeout` method is provided to escape the `parallelReduce`. 
This `timeout` is triggered when the `end` of `absoluteTime` resolves to `true`. 
Once `timeout` is triggered, `Creator` will move the DApp to a consensus step with `publish` and the DApp will return the values of `highestBidder`, `lastPrice`, and `isFirstBid`.

:::note
`timeout` methods are also used to protect participants against [non-participation attacks](https://docs.reach.sh/guide/timeout/#guide-timeout). In this DApp, the `timeout` method ensures that the auction will end after a set amount of time has passed.
:::

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 49-52
```

+ Line 49: Instructs the timeout to trigger when `absoluteTime` reaches the value stored in `end`
+ Line 50: Creator publishes to the consensus network, moving the DApp to a consensus step
+ Line 51: Creator returns the values stored in `highestBidder`, `lastPrice`, and `isFirstBid`
+ Line 52: Closes the `timeout` method

## Frontend Bidder Logic

With few exceptions, the communication model of the backend is complete. 
Now, `Bidder`'s frontend logic can be constructed to complete the communication model in the frontend.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 17-23
``` 

+ Line 17: Establishes `done` as a `let`, rather than a `const` so that the boolean is able to mutate to `true` when it is time to exit the `parallelReduce`
+ Line 18: Sets `bidders` as an empty array
+ Line 19: Creates `startBidders()`, a function that will be triggered by `auctionReady()`
+ Line 20: Stores the value in `minBid` as `bid`. 
This value is also mutable so that the `bid` value can be updated as bidders race for the NFT
+ Line 21: Creates the `runBidder()` function, 
which will be called when `API Participants` place a `bid`
+ Line 22: Randomly chooses the amount to increment the `bid` by
+ Line 23: Adds the increment to the `bid`

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 25-30
```

+ Line 25: Initializes a new account with `devnet` tokens
+ Line 26: Sets the account address as a distinguishing label in debug logs
+ Line 27: Returns a promise when the NFT is ready to be accepted by the contract
+ Line 28: Using standard JavaScript, this statement pushes the bidder and their account to the `bidders` array. 
+ Line 29: Attaches the `API Participant` to the contract
+ Line 30: Stores the balance of the `Bidder` in `getBal`

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 32-33
```

+ Line 32: Prints the `bidder` and their `bid` amount
+ Line 33: Prints the `bidder`'s balance before their `bid`

We've created the first half of the `Bidder`s communication model. 
`Bidder`s now have logic that dictates their behavior when interacting with the contract. 
This communication model allows `Bidder`s to enter a race with one another and ensures that they are aware of the highest bidder and the `bid` to beat.

A clearly defined communication model is extremely important because in web3 there is no middle man. 
Decentralized applications create an opportunity to reduce fees and increase the velocity of communications and funding. 
In this case, instead of waiting for the highest bidder to wire credit, the network tokens are escrowed the moment the `bid` is placed, and transferred the moment the auction closes.

## Assertion Insertion

Assertions in Reach can be static or dynamic. 
Static assertions, written as `assert` in the backend are always true throughout the life of the application. 
Dynamic assertions may vary in truthfulness based on the state of the DApp. Dynamic assertions in a local step use an `assume` and a `require` in a consensus step. 
However, Reach has removed the need to remember when to use which dynamic assertion with the `check` method. 
`check` will automatically determine which mode the DApp is in and use the appropriate dynamic assertion on your behalf.

Let's return to the missing pieces in the DApp and populate them with the proper assertions. 

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 26-28
```

+ Line 27: A Static Assertion: Asserts that the balance of the `nftId` will always equal the value stored in `amt`. 
If the assertion fails then the program will exit with an error message of `"balance of NFT is wrong"`.

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 37-39
```

+ Line 38: A Dynamic Assertion: Provides a local assumption that the incoming `bid` is greater than the value stored in `lastPrice`, or throw an error if false. 
The `api_` macro negates the need to use a repetitive `check` inside the `return`. 
The compiler knows that `api_` indicates that the local step and consensus step dynamic assertions are exactly the same so this is the only `check` required in this `parallelReduce`.

That concludes the backend assertions. 
Next, we'll implement a `try` and `catch` condition in the frontend.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 34-41
```

+ Line 34: Begins the `try` conditional
+ Line 35: Stores the `Bidder`'s `address` and `bid` amount in a constant 
+ Line 36: Prints a message stating who has been outbid and for how much
+ Line 37: Begins the `catch` portion of the conditional
+ Line 38: Prints a message that a bid failed because the auction ended
+ Line 40: Prints the `Bidder`'s balance after their `bid`

# Last Things

The DApp is almost complete. There are only four more small sections to complete. 
* At the end of the auction, the DApp must transfer the NFT to the highest bidder and the tokens to the Creator
* The frontend must reflect the backend's transfer pattern
* Test Bidders must be created
* Write the final `commit` and `exit` to destroy the contract

Let's resolve each of these items.

## [Transfer Patterns](#transfer)

A consensus transfer occurs when a participant, "the originator", makes a publication of a set of public values from its local state and transfers zero or more network tokens to the contract account. 
Additional consensus transfer patterns are discussed in the [reach guides](http://localhost:8080/guide/ctransfers/#guide-ctransfers). 
In the logic below, the NFT is `transfer`ed to the highest bidder at the end of the auction, and if it is not the first `bid` then the last `bid` amount is `transfer`ed to the `Creator`. 

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 54-56
```

+ Line 54: Transfers the NFT to the winning bidder
+ Line 55: If the contract is closing after at least one bid then the winning bid amount is transferred to the `Creator`
+ Line 56: `Creator` shows the winning bidder and their `bid` amount

We'll stop there and move to the frontend to create the transfer logic.

Place this block after the `Creator`s interact object closes.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 68-71
```

This block is responsible for determining which `Bidder` is interacting in the `API`, what the balances are, and broadcasting that information to the DApp's users.

+ Line 68: Creates a `for` loop that populates the `bidders` array in line 18
+ Line 69: Stores the balance of each `bidder`'s `account` and how many NFTs the `account` holds
+ Line 70: Prints the log of the user's balance and number of NFTs

This transfer pattern defined how `Bidder`s' balances are tracked and what is included in each transfer. 
A firm understanding of the economic model of your DApp is required to create decentralized applications that exchange network tokens as expected, 
and it reduces the opportunity for dishonest participants to exploit your ecosystem.

In this DApp, the NFT is transferred to the contract at the moment the auction is created. 
`Bidder`s transfer tokens to the contract the moment they place a `bid` that's higher than the previous `bid`. 
The contract returns `bid`s to `Bidders` when they've been outbid. 
This process is repeated until the auction closes. 
At that time, the contract transfers the winning `bid` amount to the `Creator` and the NFT to the winning `bidder`.

## Create API Bidders

An auction isn't much fun without bidders, so let's create a few! 

Add this block inside `startBidders`, after `runBidder` closes.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 43-49
```

+ Lines 43-45: Creates three bidders, `Alice`, `Bob`, and `Claire` who will wait to race until `runBidder` is called.
+ Lines 46-48: Features a `while` loop that increments `devnet` transaction blocks forward until `done` is `true`.

That's all there is to this step! 
On `Testnet` or `Mainnet` we would ask `Bidders` to connect to the DApp with their wallets. 
We won't cover [Provider Selection](http://localhost:8080/frontend/#ref-frontends-js-provider) in this guide, but you can learn more in the Reach docs.
This guide tests the DApp in an Algorand `devnet`, which is a Dockerized simulation of the Algorand blockchain. 
Running initial tests in `devnet` allows us to rapidly test the functionality of our DApp and work out bugs without the need to sign potentially hundreds of transactions in the process.

## Destroy the Contract

With every other task complete, the last thing to do is end the contract. 
We return to the backend to add the last two lines.

```
load: /examples/nft-auction-api/index.rsh
md5: aba879d62803fb298b7ce92187d6a489
range: 57-58
```

+ Line 57: Moves the contract into Step mode
+ Line 58: Exits and destroys the contract

And one final return to the frontend to provide the `Bidder`s with an exit.

```
load: /examples/nft-auction-api/index.mjs
md5: b76a933a8b855f0c18dab4830d3e4135
range: 72
```

+ Line 72: Assigns `done` to `true` to exit the `while` loop in line 46

That's it! 

You've successfully built a Reach DApp that simulates a live NFT Auction using `APIs` and `parallelReduce`. 
Now it's time for the fun part! 
Testing the DApp!

# Run the Reach Application

In the terminal, run the auction with 

```
$ ./reach run
```

Your `address`es and `bid` amounts will vary, but your output should look similar to:

```
Creating test account for Creator
Having creator create testing NFT
Creator sets parameters of sale: {
  nftId: BigNumber { _hex: '0xca', _isBigNumber: true },
  minBid: BigNumber { _hex: '0x1e8480', _isBigNumber: true },
  lenInBlocks: 10
}
Alice decides to bid 4.660372.
Alice balance before is 99.999
Alice out bid 0xeb5ece23d7c1c09b75050d63b242e2b204f4705d4240006504ceaab321245806 who bid 2.
Alice balance after is 95.336628
Bob decides to bid 8.88873.
Bob balance before is 99.999
Bob out bid 0xc6b60f31e3d2c100f8437a32b7eb890e97bad9416b820b5dfa13fb5797023ac0 who bid 4.660372.
Bob balance after is 91.10727
Claire decides to bid 18.22165.
Claire balance before is 99.999
Claire failed to bid, because the auction is over
Claire balance after is 99.999
Creator saw that Y23A6MPD2LAQB6CDPIZLP24JB2L3VWKBNOBAWXP2CP5VPFYCHLAHBHZ6DI bid 4.660372.
Creator saw that GOMV3XGDPQOJDPPZYHVZ5MDJGQNPQREWZ6ZXRAM3WUQLIWHVSO5LDKNTO4 bid 8.88873.
Creator saw that GOMV3XGDPQOJDPPZYHVZ5MDJGQNPQREWZ6ZXRAM3WUQLIWHVSO5LDKNTO4 won with 8.88873
Alice has 99.997198 ALGO and 0 of the NFT
Bob has 91.107452 ALGO and 1 of the NFT
Claire has 99.999099 ALGO and 0 of the NFT
```

We can see that the DApp behaves as expected. 
`Creator` creates an NFT and sets the parameters of the sale. 
`Alice`, `Bob`, and `Claire` enter a bidding race. 
Balances are updated with each `bid`, and finally, 
the auction is closed and users are presented with their balances and NFT amount. 

As a next step, we might write an [interactive command-line](http://localhost:8080/tut/rps/#tut-8) that allows the Creator to create a deadline for the auction and enable Alice, Bob, and Claire to enter their own `bid` amounts in separate terminals. 

Congratulations on successfully completing the NFT Auction using APIs with Reach. 
If you'd like to learn more about Reach then visit the [Tutorials](https://docs.reach.sh/tut/#tuts) in Reach's documentation and say hello in the Reach [Discord community](https://discord.gg/jXyTScWA).