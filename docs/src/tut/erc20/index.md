# {#erc20} ERC20

This tutorial will demonstrate the [ERC20 spec](https://eips.ethereum.org/EIPS/eip-20) in [Reach](https://docs.reach.sh/#reach-top).

It assumes prior knowledge of Reach -- we recommend completing the
[Rock, Paper, Scissors](https://docs.reach.sh/tut/rps/) tutorial and the [RSVP](https://docs.reach.sh/tut/rsvp/) tutorial first.

It also assumes you are working in a project folder called `erc20`

```cmd
$ mkdir erc20
```
```cmd
$ cd erc20
```

Create your files

```cmd
$ touch index.rsh
```
```cmd
$ touch index.mjs
```

Working directory `~/Reach/erc20`, where the reach shell script is installed in `Reach`

## Problem Analysis
Our application is going to implement the [ERC20 token spec](https://eips.ethereum.org/EIPS/eip-20) and allow functions to be called indefinitely. We'll implement the standard ERC20 functions, Views and Events. They are listed here for reference.

| ERC20 UML                                                        |
|------------------------------------------------------------------|
|                                                                  |
| Public:                                                          |
|name(): string                                                    |
|symbol(): string                                                  |
|decimals(): uint8                                                 |  
|totalSupply(): uint256                                            |
|balanceOf(account: address): uint256                              |
|transfer(to: address, amount: uint256): bool                      |
|allowance(owner: address, spender: address): uint256              |
|approve(spender: address, amount: uint256): bool                  |
|transferFrom(from: address, to: address, amount: uint256): bool   |
|                                                                  |
| Events:                                                          |
|Transfer(from: address, to: address, value: uint256)              |
|Approval(owner: address, spender: address, value: uint256)        |


### How that looks in Reach

We will go through each of these, there is no need to code this yet.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 5-31
```

#### Per the specifications:
- The `Transfer` event *MUST* trigger when tokens are transferred, including zero value transfers.

- The `Approval` event *MUST* trigger on any successful call to `approve(spender, amount)`.

Now that our problem is defined we can move on to designing our Reach program. It is best practice for Reach programs to consider the users of your application and their interaction with the contract account.

## Program Design

### Who are the users in our application?
There will be one deployer who we will implement as a `Participant` and an unbounded number of users who will interact with the contract to transfer tokens. These interactions are best implemented as `API`s.

### What are the steps of the program?
The program will first accept the token metadata and parameters and then allow our `API` member functions to be called indefinitely. This means we'll use the mighty `parallelReduce` with special considerations.

Let's define our users. 

The application starts with the Deployer providing the token metadata and deploying the contract with the first `publish`. So we add some token data definitions to our `Participant`.

This structure will allow a single address to bind to `D` and allow `A` functions to be called by other contracts or off-chain by frontends representing any number of different users. 

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 1-31
```

- Line 6 declares the token metadata as an Object with specified fields. More on [StringDyn](https://docs.reach.sh/rsh/compute/#rsh_StringDyn).
- Line 13 defines a `launched` function to notify the frontend of contract deployment. This is a good practice when building this style of Reach DApp. When done correctly, it prevents frontend interaction that relies on a deployed contract before it is complete.
- Line 15 declares an `API`.
- Lines 16-18 declare the functions our `API` members are allowed to call. These are the functions defined by the ERC20 spec.
- Line 20 declares a View that makes blockchain information more easily accessible to the frontend.
- Lines 21-26 sets the information we make available to our View.
- Line 28 declares an Event that will allow monitoring of our Reach program.
- Line 29-30 declares the two events that we will emit and their values.

:::note
The `transferFrom` method allows contracts to transfer tokens on your behalf and/or to charge fees in sub-currencies. [Source](https://eips.ethereum.org/EIPS/eip-20#simple-summary)
:::

That is all for our data definitions, so we start defining the states of our program.

As noted earlier, the first step is to have the Deployer provide the token metadata and actually deploy the contract with the first publish.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 32-38
```

Then the Deployer notifies the frontend that the contract is deployed. 
`getContract()` will return the contract value, it cannot be called until after the first `publish`.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 39-39
```

Now we can set the Views related to our token metadata. 

This information is already available, because we published it to the blockchain, but it is accessible with some difficulty. 

`View`s make this as simple as defining a function to provide the information to the frontend. 

Setting the token metadata to the `View`s, provides an easily accessible window into the consensus state.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 41-44
```

Next we'll create the `Map`s that hold balances and allowances for transfer. 

The `balances` map will be our database of ownership, so we set the balance map for the deployer to the `totalSupply`.

The `allowances` map will hold the allowed amount of tokens to be transferred by an address.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 46-49
```

Next we'll emit the Event for Transfer from the zero address.

Events emit at significant actions of the program, they allow monitoring of Reach program actions.

This event shows the token has been minted and given initial state.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 50-50
```

Before we go any further in our `rsh` file, let's jump into the frontend `mjs` file.

We'll start with necessary imports and verify the EVM connector setting. 

This DApp has two features that are not yet supported on Algorand.
- Maps with keys other than Addresses
- Dynamically sized data (StringDyn)

:::note
The upcoming Box Storage feature on Algorand will allow Reach to add support for different types of Map keys.
:::

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 1-8
```

We'll demonstrate using the frontend standard library to check `assert` statements. It will be useful to define a few helper constants. 

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 11-12
```

Next, let's write some test functions. We of course want to test that they pass when we assume they will, but we also want to check our assumptions about when we expect them to fail.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 14-21
```

Next is a helper function to verify equality. Types generated by Reach have corresponding [JavaScript type representations](https://docs.reach.sh/frontend/#p_6) that are not always the same.

UInts returned from `API`s and `Views` are represented as bigNumbers, this helper lets us use them interchangeably. 

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 23-31
```

Now let's create the [startMeUp](https://youtu.be/7JR10AThY8M) function to handle deploying our contract and any errors we may encounter. 

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 33-47
```

Then we define the zeroAddress and create our test accounts.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 49-52
```

Now we can setup our token metadata in an object to eventually be passed to the backend.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 54-62
```

Now that we have our Deployer account and token data, we can deploy the contract and send this info to the backend. 

We'll use `acc0` as the Deployer.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 64-69
```

We have all of our users created now and the contract is deployed. Now we can go back to our `.rsh` file. 

The next thing we want to do is create the functionality for our `API`s. Given that we have many users who need to do something, we want a `parallelReduce`. 

`parallelReduce` is a powerful control structure, it will allow users to repeatedly call `API`s in a looping construct.

We could use a `while` loop with a `fork`, but `parallelReduce` is a more convenient way to write it.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 52-52
```
The only values that need to be tracked in the program are the balances and allowances, so the `parallelReduce` does not need to track any values.

The `define` block of our `parallelReduce` will be used to define some helper functions.

First, a function to check the balance and set the related View.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-58
```

Why do we use `fromSome()` here?

`Map`s are the only variably sized container in Reach. This means that the value we are attempting to reference from the `balances` or `allowances` map may exist or it may not. This is generally referred to as an option type and is not unique to Reach. Option Types are an important protection against null pointer references.

Option types in Reach are represented by the type [`Maybe`](https://docs.reach.sh/rsh/compute/#maybe) which has two possibilities -- `Some` and `None`. 

Reach provides the `fromSome()` function to easily consume these `Maybe` values. It takes the `Maybe` value and a default value if `Maybe == None`. 
`fromSome(Maybe, default)` [fromSome docs](https://docs.reach.sh/rsh/compute/#rsh_fromSome).

Expanding on the `.define` block we want to also set an allowed amount of tokens and its related View.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-63
```

The last piece we need to add to our `.define` block is the `transfer_` function. 

This is one of the significant events defined in our `Events`, so we also emit an Event here.

:::note
Names suffixed with `_` are not significant other than to avoid reserved words.
:::

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-69
```

The contract account will not actually recieve tokens, so we set a simple `invariant`. 

We also want these functions to be callable indefinitely, so we set an infinite loop.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 70-71
```

Now that our loop pattern is setup, we can define our `API` member functions.

`transfer` will check for a zeroAddress transfer and verify the balance is not greater than the amount.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 72-74
```

The next piece to add to this function is the `return` call. In this case the `PAY_EXPR` is omitted and we track no values. 

We return a Boolean here to match the ERC20 spec.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 72-80
```

The `API` member function `transfer` is now complete.

Next is `transferFrom`, again we start with dynamic assertions checking for the `zeroAddress`, balances and allowances.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 81-85
```

After verifying assertions we can add the `return` to our `transferFrom` function. Again we omit the `PAY_EXPR` -- but this time update the `allowances` map and emit an `Approval` Event as required by the ERC20 spec.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 81-94
```

That completes our `transferFrom` function.

The last function to implement is the `API` member function `approve`. We'll start with its heading and a dynamic check for the `zeroAddress`.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 95-96
```

Then we add an update to the `allowances` map and emit an `Approval` Event.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 95-103
```

This ends our `.rsh` file, though because of our infinite loop -- we never actually reach `exit`.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 104-106
```

Now we can jump back to our frontend and implement some tests for our new functions.

First a function to verify assertions about the balances of our accounts and their related Views.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 71-77
```

Now a function to verify our Events.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 79-84
```

Now we'll define functions to use our `api` calls and include some calls to our `assert` functions.

First is the `transfer` function, follwed by `transferFrom`. We defined our `API` namelessly in the `.rsh` file, so we can access it here in the frontend with `ctc.a.functionName`.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 86-98
```
Notice these functions are calling our previously defined `assert` functions for verification using the frontend standard library.

Now for the `a.approve` function.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 100-104
```

Finally, we can add some tests our program!

We will test our various functions for pass/fail scenarios. Listed here are all of the calls, we won't cover inputs from each and function names denote expected behavior.

```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 107-175
```

Now we can run our application.

First set the connector mode to `ETH` with `export REACH_CONNECTOR_MODE=ETH`

Then `../reach run` and you see output that looks like this...

:::note
It may be `./reach run` depending on where you have the Reach shell script installed.
:::

```
> index
> node --experimental-modules --unhandled-rejections=strict index.mjs

Starting up...
Completed startMeUp
finised getting contract handles
Starting tests...
assertEvent complete
assertEvent call complete
assertBalances complete
assertBalances call complete
assertFail2 call complete
assertEvent complete
transfer complete
transfer call complete
assertEvent complete
assertEvent complete
transferFrom complete is true
transferFrom call complete
assertEvent complete
transfer complete
assertBalances complete
assertEvent complete
approve complete
assertBalances complete
assertEvent complete
assertEvent complete
transferFrom complete is true
assertBalances complete
assertEvent complete
assertEvent complete
transferFrom complete is true
assertBalances complete
assertEvent complete
approve complete
Finished testing!
```

Th-th-th-that's all, folks!

Below are the complete files.

First the Reach code.

 ```
  load: /examples/ERC20-simple/index.rsh
  md5: 04bb2f9ecbb7f5381fa757138203a1a7
 ```

 Now our Javascript frontend test program.

 ```
  load: /examples/ERC20-simple/index.mjs
  md5: 9658b56fe605ac4642fb72b469b8966f
 ```

