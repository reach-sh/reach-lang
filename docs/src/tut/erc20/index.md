# {#erc20} ERC20

This tutorial will demonstrate the [ERC20 spec](https://eips.ethereum.org/EIPS/eip-20) in [Reach](https://docs.reach.sh/#reach-top).

It assumes prior knowledge of Reach: we recommend completing the
[Rock, Paper, Scissors](https://docs.reach.sh/tut/rps/) tutorial and the [RSVP](https://docs.reach.sh/tut/rsvp/) tutorial first.

We assume you are working in a project folder called `erc20`:

```cmd
$ mkdir erc20
$ cd erc20
```

Start by creating your files:

```cmd
$ touch index.rsh index.mjs
```

## The ERC20 Standard

Our application is going to implement the [ERC20 token spec](https://eips.ethereum.org/EIPS/eip-20) and allow these functions to be called indefinitely.
We'll implement the standard ERC20 functions, views and events.

The functions are:
- `name(): string`
- `symbol(): string`
- `decimals(): uint8`
- `totalSupply(): uint256`
- `balanceOf(account: address): uint256`
- `transfer(to: address, amount: uint256): bool`
- `allowance(owner: address, spender: address): uint256`
- `approve(spender: address, amount: uint256): bool`
- `transferFrom(from: address, to: address, amount: uint256): bool`

And the events are:
- `Transfer(from: address, to: address, value: uint256)` which *MUST* trigger when tokens are transferred, including zero value transfers.
- `Approval(owner: address, spender: address, value: uint256)` which *MUST* trigger on any successful call to `approve(spender, amount)`.

## The ERC20 Standard in Reach

We can encode all of this in Reach in the following way:
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 5-31
```

## The Core of an ERC20 in Reach

The program will first accept the token metadata and parameters and then allow our `{!rsh} API` member functions to be called indefinitely.
This means we'll use the mighty `{!rsh} parallelReduce` with special considerations.

Let's define our users.

The application starts with the Deployer providing the token metadata and deploying the contract with the first `{!rsh} publish`.
So, we add some token data definitions to our `{!rsh} Participant`.

This structure will allow a single address to bind to `D` and allow `A` functions to be called by other contracts or off-chain by frontends representing any number of different users.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 1-31
```

- Line 6 declares the token metadata as an `{!rsh} Object` with specified fields.
  (Click on `{!rsh} StringDyn` if you've never seen it before!)
- Line 13 defines a `launched` function to notify the frontend of contract deployment.
  This is a good practice when building this style of Reach DApp. When done correctly, it prevents frontend interaction that relies on a deployed contract before it is complete.
- Line 15 declares an `{!rsh} API`.
- Lines 16-18 declare the functions our `{!rsh} API` members are allowed to call. These are the functions defined by the ERC20 spec.
- Line 20 declares a `{!rsh} View` that makes the information more easily accessible to the frontend.
- Lines 21-26 sets the information we make available to our `{!rsh} View`.
- Line 28 declares an `{!rsh} Event` that will allow monitoring of our Reach program.
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

Then, the Deployer notifies the frontend that the contract is deployed.
`{!rsh} getContract()` will return the contract value, it cannot be called until after the first `{!rsh} publish`.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 39-39
```

Now, we can set the `{!rsh} View`s related to our token metadata.

This information is already available, because we published it to the blockchain, but it is accessible with some difficulty.

`{!rsh} View`s make this as simple as defining a function to provide the information to the frontend.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 41-44
```

Next, we'll create the `{!rsh} Map`s that hold balances and allowances for transfer.
- The `balances` map will be our database of ownership, so we set the balance map for the deployer to the `totalSupply`.
- The `allowances` map will hold the allowed amount of tokens to be transferred by an address.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 46-49
```

Next, we'll emit the event for Transfer from the zero address.

Events emit at significant actions of the program, they allow monitoring of Reach program actions.

This event shows the token has been minted and given initial state.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 50-50
```

## An ERC20 Testing Frontend

Before we go any further in our Reach (`rsh`) file, let's jump into the frontend (`mjs`) file.

We'll start with necessary imports and ensure that we're running on an Ethereum-based network:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 1-8
```

Our frontend is primarily going to be a test suite, so we'll define some testing functions:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 11-12
```

Next, let's write some test functions.
We, of course want, to test that they pass when we assume they will, but we also want to check our assumptions about when we expect them to fail.
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 14-21
```

Next we define a helper function to check that values are identical.
We have a special case for numbers, because the `{!js} ===` operation does not work on `{!js} BigNumber`s.
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 23-31
```

Now, we create the [startMeUp](https://youtu.be/7JR10AThY8M) function to handle deploying our contract and any errors we may encounter.
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 33-47
```

Then, we define the `{!js} zeroAddress` and create our test accounts:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 49-52
```

Now, we can setup our token metadata in an object to eventually be passed to the backend:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 54-62
```

Now, that we have our Deployer account and token data, we can deploy the contract and send this info to the backend.
We'll use `acc0` as the Deployer:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 64-69
```

We have all of our users created now and the contract is deployed.
Now, we can go back to our `.rsh` file.

## The ERC20 APIs in Reach

The next thing we want to do is create the functionality for our `{!rsh} API`s.
Given that we have many users who need to do something, we will use a `{!rsh} parallelReduce`.
`{!rsh} parallelReduce` is a powerful control structure, it will allow users to repeatedly call `{!rsh} API`s in a looping construct.
We could use a `{!rsh} while` loop with a `{!rsh} fork`, but `{!rsh} parallelReduce` is a more convenient way to write it.
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 52-52
```
The only values that need to be tracked in the program are the balances and allowances, so the `{!rsh} parallelReduce` does not track any values.

The `{!rsh} define` block of our `{!rsh} parallelReduce` will be used to define some helper functions.

First, a function to check the balance and set the related View.

```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-58
```

:::note
Why do we use `{!rsh} fromSome()` here?

The keys of a `{!rsh} Map` (i.e. `balances` and `allowances`) may not exist, so Reach returns an option type:
`{!rsh} Maybe` which has two possibilities: `{!rsh} Some` (present) and `{!rsh} None` (not present).
Reach provides the `{!rsh} fromSome()` function to easily consume these `{!rsh} Maybe` values, returning a default value in the `{!rsh} None` case.
:::

Next, we also set the allowances of each account:
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-63
```

Finally, we define a helper function for doing transfers, which emits the `Transfer` event, as required by the ERC20 spec.
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 53-69
```

After the `{!rsh} .define` block of the `{!rsh} parallelReduce`, we set up the invariant (the balance never changes from zero) and the termination condition (never):
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 70-71
```

---

We can now define our `{!rsh} API` member functions.

The `transfer` is basic wrapper around the `transfer_` function with a few checks:
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 72-80
```

Next is the `transferFrom` function, which is like `transfer` but uses the `allowances` concept from the ERC20 standard:
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 81-94
```
In this case, we emit the `Approval` event, because the allowances changed.

Finally, we have the `approve` function, which sets the allowances:
```
load: /examples/ERC20-simple/index.rsh
md5: 04bb2f9ecbb7f5381fa757138203a1a7
range: 95-106
```
This too emits an `Approval` event.

(The final few lines are outside of the `{!rsh} parallelReduce` and simply `{!rsh} exit`, which will never happen because the `{!rsh} while` condition is `{!rsh} true`.

## The Rest of the ERC20 Test Suite

We can jump back to our frontend and implement some tests for our new functions.

First, we define a function to verify assertions about the balances of our accounts:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 71-77
```

Next, we define a function to ensure that an event actually occurred:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 79-84
```

Finally, we define wrappers around each of the `{!rsh} API` calls, but wrap them in a few test assertions related to the events that we expect to be generated:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 86-104
```

---

Finally, we can add some tests our program!
Each test is annotated with some explanation of what its purpose is:
```
load: /examples/ERC20-simple/index.mjs
md5: 9658b56fe605ac4642fb72b469b8966f
range: 107-175
```

## Running the ERC20 Test Suite

Now, we can run our application.
Remember, this is only designed to work on the Ethereum network, so we'll run with:
```cmd
$ REACH_CONNECTOR_MODE=ETH ./reach run
```

The output looks something like:
```
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

Our test suite was 175 lines and our Reach application was 106 lines.
Great job!
